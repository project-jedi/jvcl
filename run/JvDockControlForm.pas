{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockControlForm.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-12-31

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDockControlForm;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, Menus,
  ExtCtrls, ComCtrls,
  {$IFDEF USEJVCL}
  JvComponent, JvAppStorage,
  {$ENDIF USEJVCL}
  JvDockTree, JvDockSupportClass, JvDockSupportControl;

const
  JvDockState_Unknown = 0;
  JvDockState_Docking = 1;
  JvDockState_Floating = 2;

type
  TJvDockSplitterSize = 0..32767;

  TJvDockBaseControl = class;
  TJvDockServer = class;
  TJvDockClient = class;
  TJvDockConjoinPanel = class;
  TJvDockTabPageControl = class;
  TJvDockConjoinHostForm = class;
  TJvDockTabHostForm = class;

  TJvDockSplitter = class(TJvDockCustomPanelSplitter)
  private
    FDockServer: TJvDockServer;
    function GetSplitterIndex: Integer;
  protected
    function FindControl: TControl; override;
    property DockServer: TJvDockServer read FDockServer write FDockServer;
  public
    constructor Create(AOwner: TComponent); override;
    property SplitterIndex: Integer read GetSplitterIndex;
  end;

  TJvDockSetDockPanelSizeFrom = (sdfDockPanel, sdfClient);

  TJvDockPanel = class(TJvDockCustomPanel)
  private
    FDockServer: TJvDockServer;
    function GetPanelIndex: Integer;
  protected
    procedure SetDockServer(const Value: TJvDockServer); virtual;
    procedure ReloadDockedControl(const AControlName: string;
      var AControl: TControl); override;

    procedure CustomStartDock(var Source: TJvDockDragDockObject); override;
    procedure CustomGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomPositionDockRect(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomEndDock(Target: TObject; X, Y: Integer); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;

    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;

    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;

    procedure ShowDockPanel(MakeVisible: Boolean; Client: TControl;
      PanelSizeFrom: TJvDockSetDockPanelSizeFrom = sdfClient); virtual;
    procedure ResetPosition;
    property PanelIndex: Integer read GetPanelIndex;
    property DockServer: TJvDockServer read FDockServer write SetDockServer;
  end;

  TJvDockAdvPanel = class(TJvDockPanel)
  private
    procedure CMUnDockClient(var Msg: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  TJvDockPanelClass = class of TJvDockPanel;
  TJvDockSplitterClass = class of TJvDockSplitter;
  TJvDockConjoinPanelClass = class of TJvDockConjoinPanel;
  TJvDockTabClass = class of TJvDockTabPageControl;

  TJvDockSplitterStyle = class(TPersistent)
  private
    FSplitter: TJvDockSplitter;
    FDockServer: TJvDockServer;
    FColor: TColor;
    FCursor: TCursor;
    FParentColor: Boolean;
    FResizeStyle: TResizeStyle;
    FSize: TJvDockSplitterSize;
    FMinSize: TJvDockSplitterSize;
    procedure SetColor(const Value: TColor);
    procedure SetCursor(const Value: TCursor);
    procedure SetParentColor(const Value: Boolean);
    procedure SetResizeStyle(const Value: TResizeStyle);
    procedure SetSize(const Value: TJvDockSplitterSize);
    procedure SetMinSize(const Value: TJvDockSplitterSize);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AssignToSplitter(Dest: TJvDockSplitter);
    procedure SetSplitterStyle;
    property Splitter: TJvDockSplitter read FSplitter write FSplitter;
  public
    constructor Create(ASplitter: TJvDockSplitter; ACursor: TCursor); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Cursor: TCursor read FCursor write SetCursor;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property ResizeStyle: TResizeStyle read FResizeStyle write SetResizeStyle default rsPattern;
    property Size: TJvDockSplitterSize read FSize write SetSize default 3;
    property MinSize: TJvDockSplitterSize read FMinSize write SetMinSize default 30;
  end;

  TJvDockBasicStyle = class;

  TJvDockBasicServerOption = class(TPersistent)
  private
    FDockStyle: TJvDockBasicStyle;
  protected
    procedure ResetDockControlOption; virtual; abstract;
    procedure ResetDockServerOption(ADockServer: TJvDockServer); virtual;
    procedure ResetDockClientOption(ADockClient: TJvDockClient); virtual;
    property DockStyle: TJvDockBasicStyle read FDockStyle write FDockStyle;
  public
    constructor Create(ADockStyle: TJvDockBasicStyle); virtual;
    procedure Assign(Source: TPersistent); override;
  end;

  TJvDockGrabbersSize = 1..MaxInt;
  TJvDockSplitterWidth = 1..MaxInt;

  TJvDockBasicConjoinServerOption = class(TJvDockBasicServerOption)
  private
    FGrabbersSize: TJvDockGrabbersSize;
    FSplitterWidth: TJvDockSplitterWidth;
  protected
    procedure SeTJvDockGrabbersSize(const Value: TJvDockGrabbersSize); virtual;
    procedure SetDockSplitterWidth(const Value: TJvDockSplitterWidth); virtual;

    procedure ResetDockControlOption; override;
    procedure ResetDockServerOption(ADockServer: TJvDockServer); override;
    procedure ResetDockClientOption(ADockClient: TJvDockClient); override;

    procedure ResetDockPanel(APanel: TJvDockPanel); virtual;
    procedure ResetConjoinPanel(APanel: TJvDockConjoinPanel); virtual;
  public
    constructor Create(ADockStyle: TJvDockBasicStyle); override;
    procedure Assign(Source: TPersistent); override;
    procedure SetDockGrabbersSize_WithoutChangeSystemInfo(const Value: TJvDockGrabbersSize);
    procedure SetDockSplitterWidth_WithoutChangeSystemInfo(const Value: TJvDockSplitterWidth);
  published
    property GrabbersSize: TJvDockGrabbersSize read FGrabbersSize write SeTJvDockGrabbersSize;
    property SplitterWidth: TJvDockSplitterWidth read FSplitterWidth write SetDockSplitterWidth;
  end;

  TJvDockBasicTabServerOption = class(TJvDockBasicServerOption)
  private
    FTabPosition: TTabPosition;
    FHotTrack: Boolean;
  protected
    procedure SetTabPosition(const Value: TTabPosition); virtual;
    procedure SetHotTrack(const Value: Boolean); virtual;

    procedure ResetDockControlOption; override;
    procedure ResetDockServerOption(ADockServer: TJvDockServer); override;
    procedure ResetDockClientOption(ADockClient: TJvDockClient); override;

    procedure ResetTabPageControl(APage: TJvDockTabPageControl); virtual;
  public
    constructor Create(ADockStyle: TJvDockBasicStyle); override;
    procedure Assign(Source: TPersistent); override;
  published
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition default tpTop;
  end;

  TJvDockBasicConjoinServerOptionClass = class of TJvDockBasicConjoinServerOption;
  TJvDockBasicTabServerOptionClass = class of TJvDockBasicTabServerOption;

  {$IFDEF USEJVCL}
  TJvDockBasicStyle = class(TJvComponent)
  {$ELSE}
  TJvDockBasicStyle = class(TComponent)
  {$ENDIF USEJVCL}
  private
    FDockPanelClass: TJvDockPanelClass;
    FDockSplitterClass: TJvDockSplitterClass;
    FConjoinPanelClass: TJvDockConjoinPanelClass;
    FTabDockClass: TJvDockTabClass;
    FDockPanelTreeClass: TJvDockTreeClass;
    FDockPanelZoneClass: TJvDockZoneClass;
    FConjoinPanelTreeClass: TJvDockTreeClass;
    FConjoinPanelZoneClass: TJvDockZoneClass;
    FConjoinServerOptionClass: TJvDockBasicConjoinServerOptionClass;
    FTabServerOptionClass: TJvDockBasicTabServerOptionClass;

    FConjoinServerOption: TJvDockBasicConjoinServerOption;
    FTabServerOption: TJvDockBasicTabServerOption;

    FParentForm: TForm;
    FOldWindowProc: TWndMethod;

    FDockBaseControlList: TList;

    function GetCount: Integer;
    function GetDockBaseControlLists(Index: Integer): TJvDockBaseControl;
  protected
    procedure FormStartDock(DockClient: TJvDockClient; var Source: TJvDockDragDockObject); virtual;
    procedure FormGetSiteInfo(Source: TJvDockDragDockObject; DockClient: TJvDockClient; Client: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); virtual;
    procedure FormDockOver(DockClient: TJvDockClient; Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); virtual;
    procedure FormPositionDockRect(DockClient: TJvDockClient; Source: TJvDockDragDockObject); virtual;
    procedure FormDockDrop(DockClient: TJvDockClient; Source: TJvDockDragDockObject; X, Y: Integer); virtual;
    procedure FormEndDock(DockClient: TJvDockClient; Target: TObject; X, Y: Integer); virtual;
    function FormUnDock(DockClient: TJvDockClient; NewTarget: TWinControl; Client: TControl): Boolean; virtual;
    procedure FormGetDockEdge(DockClient: TJvDockClient; Source: TJvDockDragDockObject;
      MousePos: TPoint; var DropAlign: TAlign); virtual;

    procedure CreateConjoinServerOption(var Option: TJvDockBasicConjoinServerOption); virtual;
    procedure CreateTabServerOption(var Option: TJvDockBasicTabServerOption); virtual;
    procedure CreateServerOption; virtual;
    procedure FreeServerOption; virtual;

    procedure SetDockBaseControl(IsCreate: Boolean; DockBaseControl: TJvDockBaseControl); virtual;
    function DockServerWindowProc(DockServer: TJvDockServer; var Msg: TMessage): Boolean; virtual;
    function DockClientWindowProc(DockClient: TJvDockClient; var Msg: TMessage): Boolean; virtual;
    procedure ParentFormWindowProc(var Msg: TMessage); virtual;
    procedure AddDockBaseControl(ADockBaseControl: TJvDockBaseControl); virtual;
    procedure RemoveDockBaseControl(ADockBaseControl: TJvDockBaseControl); virtual;
    procedure SetConjoinServerOption(
      const Value: TJvDockBasicConjoinServerOption); virtual;
    procedure SetTabServerOption(const Value: TJvDockBasicTabServerOption); virtual;
    function GetConjoinServerOption: TJvDockBasicConjoinServerOption; virtual;
    function GetTabServerOption: TJvDockBasicTabServerOption; virtual;

    procedure AssignConjoinServerOption(APanel: TJvDockCustomPanel); virtual;

    procedure AssignTabServerOption(APage: TJvDockTabPageControl); virtual;
    procedure Loaded; override;
    property DockBaseControlList: TList read FDockBaseControlList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function CanSetEnableDocked(ADockBaseControl: TJvDockBaseControl): Boolean; virtual;
    function CanSetLeftDocked(ADockBaseControl: TJvDockBaseControl): Boolean; virtual;
    function CanSetRightDocked(ADockBaseControl: TJvDockBaseControl): Boolean; virtual;
    function CanSetTopDocked(ADockBaseControl: TJvDockBaseControl): Boolean; virtual;
    function CanSetBottomDocked(ADockBaseControl: TJvDockBaseControl): Boolean; virtual;
    function CanSetEachOtherDocked(ADockBaseControl: TJvDockBaseControl): Boolean; virtual;
    {$IFNDEF USEJVCL}
    function GetControlName: string; virtual;
    function GetDockStyleVersion: string; virtual;
    {$ENDIF USEJVCL}

    procedure ResetCursor(Source: TJvDockDragDockObject); virtual;

    function GetDockState(DockClient: TJvDockClient): Integer; virtual;
    procedure ResetDockControlConjoinOption;
    procedure ResetDockControlTabOption;

    procedure ShowDockForm(ADockClient: TJvDockClient); virtual;
    procedure HideDockForm(ADockClient: TJvDockClient); virtual;
    function GetDockFormVisible(ADockClient: TJvDockClient): Boolean; virtual;

    property Count: Integer read GetCount;
    property DockBaseControlLists[Index: Integer]: TJvDockBaseControl read GetDockBaseControlLists;

    procedure RestoreClient(DockClient: TJvDockClient); virtual;

    {$IFNDEF USEJVCL}
    property Version: string read GetDockStyleVersion;
    property ControlName: string read GetControlName;
    {$ENDIF USEJVCL}

    property DockPanelClass: TJvDockPanelClass
      read FDockPanelClass write FDockPanelClass;
    property DockSplitterClass: TJvDockSplitterClass
      read FDockSplitterClass write FDockSplitterClass;
    property ConjoinPanelClass: TJvDockConjoinPanelClass
      read FConjoinPanelClass write FConjoinPanelClass;
    property TabDockClass: TJvDockTabClass
      read FTabDockClass write FTabDockClass;
    property DockPanelTreeClass: TJvDockTreeClass
      read FDockPanelTreeClass write FDockPanelTreeClass;
    property DockPanelZoneClass: TJvDockZoneClass
      read FDockPanelZoneClass write FDockPanelZoneClass;
    property ConjoinPanelTreeClass: TJvDockTreeClass
      read FConjoinPanelTreeClass write FConjoinPanelTreeClass;
    property ConjoinPanelZoneClass: TJvDockZoneClass
      read FConjoinPanelZoneClass write FConjoinPanelZoneClass;

    property ConjoinServerOptionClass: TJvDockBasicConjoinServerOptionClass
      read FConjoinServerOptionClass write FConjoinServerOptionClass;
    property TabServerOptionClass: TJvDockBasicTabServerOptionClass
      read FTabServerOptionClass write FTabServerOptionClass;

    property ParentForm: TForm read FParentForm write FParentForm;

    property ConjoinServerOption: TJvDockBasicConjoinServerOption
      read GetConjoinServerOption write SetConjoinServerOption;
    property TabServerOption: TJvDockBasicTabServerOption
      read GetTabServerOption write SetTabServerOption;
  end;

  TJvDockAdvStyle = class(TJvDockBasicStyle)
  protected
    function DockClientWindowProc(DockClient: TJvDockClient; var Msg: TMessage): Boolean; override;
  end;

  {$IFDEF USEJVCL}
  TJvDockBaseControl = class(TJvComponent)
  {$ELSE}
  TJvDockBaseControl = class(TComponent)
  {$ENDIF USEJVCL}
  private
    FEnableDock: Boolean;
    FLeftDock: Boolean;
    FTopDock: Boolean;
    FRightDock: Boolean;
    FBottomDock: Boolean;
    FEachOtherDock: Boolean;
    FDockStyle: TJvDockBasicStyle;

    FOldOnClose: TCloseEvent;
    FOldOnCreate: TNotifyEvent;

    FParentForm: TForm;

    FOldWindowProc: TWndMethod;
  protected
    procedure Loaded; override;
    procedure SetParentComponent(Value: TComponent); override;

    function CanSetEnableDocked: Boolean; virtual;
    function CanSetLeftDocked: Boolean; virtual;
    function CanSetRightDocked: Boolean; virtual;
    function CanSetTopDocked: Boolean; virtual;
    function CanSetBottomDocked: Boolean; virtual;
    function CanSetEachOtherDocked: Boolean; virtual;

    procedure DoFormOnClose(Sender: TObject; var Action: TCloseAction); virtual;
    procedure DoFormOnCreate(Sender: TObject); virtual;

    procedure SetBottomDock(const Value: Boolean); virtual;
    procedure SetEachOtherDock(const Value: Boolean); virtual;
    procedure SetEnableDock(const Value: Boolean); virtual;
    procedure SetLeftDock(const Value: Boolean); virtual;
    procedure SetRightDock(const Value: Boolean); virtual;
    procedure SetTopDock(const Value: Boolean); virtual;
    procedure SetDockStyle(const Value: TJvDockBasicStyle); virtual;

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure WindowProc(var Msg: TMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ParentForm: TForm read FParentForm;

    {$IFNDEF USEJVCL}
    function GetDockStyleVersion: string; virtual;
    {$ENDIF USEJVCL}

    property EnableDock: Boolean read FEnableDock write SetEnableDock default True;
    property LeftDock: Boolean read FLeftDock write SetLeftDock default True;
    property TopDock: Boolean read FTopDock write SetTopDock default True;
    property RightDock: Boolean read FRightDock write SetRightDock default True;
    property BottomDock: Boolean read FBottomDock write SetBottomDock default True;
    property EachOtherDock: Boolean read FEachOtherDock write SetEachOtherDock default True;
    {$IFNDEF USEJVCL}
    property Version: string read GetDockStyleVersion;
    {$ENDIF USEJVCL}
    property DockStyle: TJvDockBasicStyle read FDockStyle write SetDockStyle;
  end;

  TJvDockGetClientAlignSizeEvent = procedure(Align: TAlign; var Value: Integer) of object;
  TJvDockFinishSetDockPanelSizeEvent = procedure(DockPanel: TJvDockPanel) of object;

  TJvDockServer = class(TJvDockBaseControl)
  private
    FDockPanelClass: TJvDockPanelClass;
    FLeftDockPanel: TJvDockPanel;
    FTopDockPanel: TJvDockPanel;
    FBottomDockPanel: TJvDockPanel;
    FRightDockPanel: TJvDockPanel;

    FDockSplitterClass: TJvDockSplitterClass;
    FLeftSplitter: TJvDockSplitter;
    FTopSplitter: TJvDockSplitter;
    FBottomSplitter: TJvDockSplitter;
    FRightSplitter: TJvDockSplitter;

    FLeftSplitterStyle: TJvDockSplitterStyle;
    FTopSplitterStyle: TJvDockSplitterStyle;
    FRightSplitterStyle: TJvDockSplitterStyle;
    FBottomSplitterStyle: TJvDockSplitterStyle;

    FOnGetClientAlignSize: TJvDockGetClientAlignSizeEvent;
    FOnFinishSetDockPanelSize: TJvDockFinishSetDockPanelSizeEvent;
    FAutoFocusDockedForm: boolean;

    procedure CreateDockPanelAndSplitter;

    procedure CreateSplitterStyle;
    procedure DestroySplitterStyle;

    procedure SetSplitterStyle;
    procedure SetLeftSplitterStyle(const Value: TJvDockSplitterStyle);
    procedure SetTopSplitterStyle(const Value: TJvDockSplitterStyle);
    procedure SetRightSplitterStyle(const Value: TJvDockSplitterStyle);
    procedure SetBottomSplitterStyle(const Value: TJvDockSplitterStyle);

    function GetDockPanel(Index: Integer): TJvDockPanel;

    function GetDockSplitter(Index: Integer): TJvDockSplitter;
    procedure DoGetClientAlignControl(Align: TAlign; var Value: Integer);
    function GetDockPanelWithAlign(Index: TAlign): TJvDockPanel;
    function GetDockSplitterWithAlign(Index: TAlign): TJvDockSplitter;
  protected
    procedure Loaded; override;

    procedure DoFinishSetDockPanelSize(DockPanel: TJvDockPanel);
    procedure DoFloatDockClients(DockPanel: TJvDockPanel);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetBottomDock(const Value: Boolean); override;
    procedure SetEnableDock(const Value: Boolean); override;
    procedure SetLeftDock(const Value: Boolean); override;
    procedure SetRightDock(const Value: Boolean); override;
    procedure SetTopDock(const Value: Boolean); override;

    procedure WMActivate(var Msg: TWMActivate);
    procedure WindowProc(var Msg: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetClientAlignControl(Align: TAlign): Integer;

    property LeftDockPanel: TJvDockPanel read FLeftDockPanel;
    property RightDockPanel: TJvDockPanel read FRightDockPanel;
    property TopDockPanel: TJvDockPanel read FTopDockPanel;
    property BottomDockPanel: TJvDockPanel read FBottomDockPanel;

    property LeftSplitter: TJvDockSplitter read FLeftSplitter;
    property RightSplitter: TJvDockSplitter read FRightSplitter;
    property TopSplitter: TJvDockSplitter read FTopSplitter;
    property BottomSplitter: TJvDockSplitter read FBottomSplitter;

    property DockPanel[Index: Integer]: TJvDockPanel read GetDockPanel;
    property DockPanelWithAlign[Index: TAlign]: TJvDockPanel read GetDockPanelWithAlign;
    property DockSplitter[Index: Integer]: TJvDockSplitter read GetDockSplitter;
    property DockSplitterWithAlign[Index: TAlign]: TJvDockSplitter read GetDockSplitterWithAlign;
    {$IFNDEF USEJVCL}
    property Version: string read GetDockStyleVersion;
    {$ENDIF USEJVCL}
  published
    property LeftSplitterStyle: TJvDockSplitterStyle read FLeftSplitterStyle write SetLeftSplitterStyle;
    property TopSplitterStyle: TJvDockSplitterStyle read FTopSplitterStyle write SetTopSplitterStyle;
    property RightSplitterStyle: TJvDockSplitterStyle read FRightSplitterStyle write SetRightSplitterStyle;
    property BottomSplitterStyle: TJvDockSplitterStyle read FBottomSplitterStyle write SetBottomSplitterStyle;
    property AutoFocusDockedForm: boolean read FAutoFocusDockedForm write FAutoFocusDockedForm default True;

    property OnGetClientAlignSize: TJvDockGetClientAlignSizeEvent
      read FOnGetClientAlignSize write FOnGetClientAlignSize;
    property OnFinishSetDockPanelSize: TJvDockFinishSetDockPanelSizeEvent
      read FOnFinishSetDockPanelSize write FOnFinishSetDockPanelSize;

    property EnableDock;
    property LeftDock;
    property TopDock;
    property RightDock;
    property BottomDock;
    property DockStyle;
  end;

  TJvDockMouseStation = (msFloat, msConjoin, msTabPage);

  TJvDockNCButtonEvent = procedure(DockClient: TJvDockClient; Button: TMouseButton;
    X, Y: Smallint; HitTest: Longint; MouseStation: TJvDockMouseStation) of object;
  TJvDockNCButtonDownEvent = TJvDockNCButtonEvent;
  TJvDockNCButtonUpEvent = TJvDockNCButtonEvent;
  TJvDockNCButtonDblClkEvent = TJvDockNCButtonEvent;
  TJvDockNCMouseMoveEvent = procedure(DockClient: TJvDockClient;
    X, Y: Smallint; HitTest: Longint; MouseStation: TJvDockMouseStation) of object;
  TJvDockPaintDockEvent = procedure(Canvas: TCanvas;
    Control: TControl; const ARect: TRect) of object;
  TJvDockPaintDockGrabberEvent = TJvDockPaintDockEvent;
  TJvDockPaintDockSplitterEvent = TJvDockPaintDockEvent;
  TJvDockFormHintEvent = procedure(HTFlag: Integer; var HintStr: string; var CanShow: Boolean) of object;

  TJvDockClient = class(TJvDockBaseControl)
  private
    FConjoinPanelClass: TJvDockConjoinPanelClass;
    FTabDockClass: TJvDockTabClass;
    FParentVisible: Boolean;
    FNCPopupMenu: TPopupMenu;
    FDirectDrag: Boolean;
    FShowHint: Boolean;
    FCanFloat: Boolean;
    FRelativeServer: TJvDockServer;
    FDockLevel: Integer;
    FEnableCloseButton: Boolean;
    FOnNCButtonDown: TJvDockNCButtonDownEvent;
    FOnNCButtonUp: TJvDockNCButtonUpEvent;
    FOnNCMouseMove: TJvDockNCMouseMoveEvent;
    FOnNCButtonDblClk: TJvDockNCButtonDblClkEvent;
    FOnPaintDockGrabber: TJvDockPaintDockGrabberEvent;
    FOnPaintDockSplitter: TJvDockPaintDockSplitterEvent;
    FOnFormShowHint: TJvDockFormHintEvent;
    FOnFormShow: TNotifyEvent;
    FOnFormHide: TNotifyEvent;
    FCurrentDockSite: TWinControl;
    FLastDockSite: TWinControl;
    FUnDockLeft: Integer;
    FUnDockTop: Integer;
    FVSPaneWidth: Integer;
    procedure SetParentVisible(const Value: Boolean);
    function GetLRDockWidth: Integer;
    function GetTBDockHeight: Integer;
    procedure SetLRDockWidth(const Value: Integer);
    procedure SetTBDockHeight(const Value: Integer);
    procedure SetNCPopupMenu(const Value: TPopupMenu);
    procedure WMNCLButtonDown(var Msg: TWMNCHitMessage);
    procedure WMNCLButtonUp(var Msg: TWMNCHitMessage);
    procedure WMNCLButtonDblClk(var Msg: TWMNCHitMessage);
    procedure WMNCMButtonDown(var Msg: TWMNCHitMessage);
    procedure WMNCMButtonUp(var Msg: TWMNCHitMessage);
    procedure WMNCMButtonDblClk(var Msg: TWMNCHitMessage);
    procedure WMNCRButtonDown(var Msg: TWMNCHitMessage);
    procedure WMNCRButtonUp(var Msg: TWMNCHitMessage);
    procedure WMNCRButtonDblClk(var Msg: TWMNCHitMessage);
    procedure WMNCMouseMove(var Msg: TWMNCHitMessage);
    procedure CMVisibleChanged(var Msg: TMessage);
    procedure SetCurrentDockSite(const Value: TWinControl);
    procedure SetLastDockSite(const Value: TWinControl);
    procedure SetVSPaneWidth(const Value: Integer);
    procedure SetUnDockLeft(const Value: Integer);
    procedure SetUnDockTop(const Value: Integer);
    function GetDockState: Integer;
    procedure SetCanFloat(const Value: Boolean);
    procedure SetRelativeServer(const Value: TJvDockServer);
    procedure SetDockLevel(const Value: Integer);
    procedure SetEnableCloseButton(const Value: Boolean);
  protected
    procedure Loaded; override;
    procedure DoMenuPopup(X, Y: Integer); virtual;

    procedure Deactivate; virtual;
    procedure Activate; virtual;

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    procedure DoFloatDockClients(PanelAlign: TAlign);
    procedure DoFloatDockEachOther;
    procedure SetBottomDock(const Value: Boolean); override;
    procedure SetEachOtherDock(const Value: Boolean); override;
    procedure SetEnableDock(const Value: Boolean); override;
    procedure SetLeftDock(const Value: Boolean); override;
    procedure SetRightDock(const Value: Boolean); override;
    procedure SetTopDock(const Value: Boolean); override;

    procedure DoFormOnClose(Sender: TObject;
      var Action: TCloseAction); override;

    procedure WMSize(var Msg: TWMSize);
    procedure WMActivate(var Msg: TWMActivate);

    procedure WindowProc(var Msg: TMessage); override;
    property RelativeServer: TJvDockServer read FRelativeServer write SetRelativeServer default nil;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FormStartDock(var Source: TJvDockDragDockObject); virtual;
    procedure FormPositionDockRect(Source: TJvDockDragDockObject); virtual;
    procedure FormDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); virtual;
    procedure FormDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); virtual;
    procedure FormEndDock(Target: TObject; X, Y: Integer); virtual;
    function FormUnDock(NewTarget: TWinControl; Client: TControl): Boolean; virtual;
    procedure FormGetSiteInfo(Source: TJvDockDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); virtual;
    procedure FormGetDockEdge(Source: TJvDockDragDockObject; MousePos: TPoint; var DropAlign: TAlign); virtual;

    procedure Assign(Source: TPersistent); override;

    procedure MakeShowEvent;
    procedure MakeHideEvent;

    function CreateConjoinPanelClass(ConjoinHost: TForm): TJvDockConjoinPanel;
    function CreateTabDockClass(TabHost: TForm): TJvDockTabPageControl;
    function CreateConjoinHostAndDockControl(Control1, Control2: TControl;
      DockType: TAlign): TJvDockConjoinHostForm; virtual;
    function CreateTabHostAndDockControl(Control1, Control2: TControl): TJvDockTabHostForm; virtual;

    procedure DoNCButtonDown(Msg: TWMNCHitMessage; Button: TMouseButton;
      MouseStation: TJvDockMouseStation); virtual;
    procedure DoNCButtonUp(Msg: TWMNCHitMessage; Button: TMouseButton;
      MouseStation: TJvDockMouseStation); virtual;
    procedure DoNCMouseMove(Msg: TWMNCHitMessage;
      MouseStation: TJvDockMouseStation); virtual;
    procedure DoNCButtonDblClk(Msg: TWMNCHitMessage; Button: TMouseButton;
      MouseStation: TJvDockMouseStation); virtual;

    procedure DoPaintDockGrabber(Canvas: TCanvas;
      Control: TControl; const ARect: TRect);
    procedure DoPaintDockSplitter(Canvas: TCanvas;
      Control: TControl; const ARect: TRect);
    procedure DoFormShowHint(HTFlag: Integer; var HintStr: string; var CanShow: Boolean);

    procedure ShowParentForm;
    procedure HideParentForm;

    procedure RestoreChild;

    property VSPaneWidth: Integer read FVSPaneWidth write SetVSPaneWidth;
    property ParentVisible: Boolean read FParentVisible write SetParentVisible;
    property CurrentDockSite: TWinControl read FCurrentDockSite write SetCurrentDockSite;
    property LastDockSite: TWinControl read FLastDockSite write SetLastDockSite;
    property UnDockLeft: Integer read FUnDockLeft write SetUnDockLeft;
    property UnDockTop: Integer read FUnDockTop write SetUnDockTop;
    property DockState: Integer read GetDockState;
  published
    property OnFormShow: TNotifyEvent read FOnFormShow write FOnFormShow;
    property OnFormHide: TNotifyEvent read FOnFormHide write FOnFormHide;
    property OnNCButtonDown: TJvDockNCButtonDownEvent read FOnNCButtonDown write FOnNCButtonDown;
    property OnNCButtonUp: TJvDockNCButtonUpEvent read FOnNCButtonUp write FOnNCButtonUp;
    property OnNCMouseMove: TJvDockNCMouseMoveEvent read FOnNCMouseMove write FOnNCMouseMove;
    property OnNCButtonDblClk: TJvDockNCButtonDblClkEvent read FOnNCButtonDblClk write FOnNCButtonDblClk;
    property OnPaintDockGrabber: TJvDockPaintDockGrabberEvent read FOnPaintDockGrabber write FOnPaintDockGrabber;
    property OnPaintDockSplitter: TJvDockPaintDockSplitterEvent read FOnPaintDockSplitter write FOnPaintDockSplitter;
    property OnFormShowHint: TJvDockFormHintEvent read FOnFormShowHint write FOnFormShowHint;

    property LRDockWidth: Integer read GetLRDockWidth write SetLRDockWidth;
    property TBDockHeight: Integer read GetTBDockHeight write SetTBDockHeight;
    property NCPopupMenu: TPopupMenu read FNCPopupMenu write SetNCPopupMenu;
    property DirectDrag: Boolean read FDirectDrag write FDirectDrag;
    property ShowHint: Boolean read FShowHint write FShowHint;
    property CanFloat: Boolean read FCanFloat write SetCanFloat default True;
    property DockLevel: Integer read FDockLevel write SetDockLevel default 0;
    property EnableCloseButton: Boolean read FEnableCloseButton write SetEnableCloseButton;
    property EnableDock;
    property LeftDock;
    property TopDock;
    property RightDock;
    property BottomDock;
    property EachOtherDock;
    property DockStyle;
  end;

  TJvDockConjoinPanel = class(TJvDockCustomPanel)
  private
    FDockClient: TJvDockClient;
    function GetParentForm: TJvDockConjoinHostForm;
    procedure CMUnDockClient(var Msg: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure ReloadDockedControl(const AControlName: string;
      var AControl: TControl); override;

    procedure CustomStartDock(var Source: TJvDockDragDockObject); override;
    procedure CustomGetSiteInfo(Source: TJvDockDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomPositionDockRect(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomEndDock(Target: TObject; X, Y: Integer); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;

    procedure DoDockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    property DockClient: TJvDockClient read FDockClient write FDockClient;
    property ParentForm: TJvDockConjoinHostForm read GetParentForm;
  end;

  TJvDockAdvConjoinPanel = class(TJvDockConjoinPanel)
  private
    procedure CMUnDockClient(var Msg: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  TJvDockTabPageControl = class(TJvDockPageControl)
  private
    FDockClient: TJvDockClient;
    FVersion: Integer;
    function GetParentForm: TJvDockTabHostForm;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure ReloadDockedControl(const AControlName: string;
      var AControl: TControl); override;
    procedure CustomStartDock(var Source: TJvDockDragDockObject); override;
    procedure CustomGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl; var InfluenceRect: TRect; MousePos:
      TPoint;
      var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomPositionDockRect(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomEndDock(Target: TObject; X, Y: Integer); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    procedure CustomGetDockEdge(Source: TJvDockDragDockObject; MousePos: TPoint; var DropAlign: TAlign); override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    property DockClient: TJvDockClient read FDockClient write FDockClient;
    property ParentForm: TJvDockTabHostForm read GetParentForm;
    property TabPosition;
  end;

  TJvDockAdvTabPageControl = class(TJvDockTabPageControl)
  private
    procedure CMUnDockClient(var Msg: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    destructor Destroy; override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  TJvDockableForm = class(TForm)
  private
    FDockClient: TJvDockClient;
    FDockableControl: TWinControl;
    FUnDockControl: TControl;
    FFloatingChild: TControl;
    function GetDockableControl: TWinControl;
    procedure SetDockableControl(const Value: TWinControl);
    procedure SetUnDockControl(const Value: TControl);
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DockClient: TJvDockClient read FDockClient write FDockClient;
    property DockableControl: TWinControl read GetDockableControl write SetDockableControl;
    property UnDockControl: TControl read FUnDockControl write SetUnDockControl;
    property FloatingChild: TControl read FFloatingChild;
  end;

  TJvDockConjoinHostForm = class(TJvDockableForm)
  private
    FPanel: TJvDockConjoinPanel;
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create(AOwner: TComponent); override;
    property DockClient;
    property Panel: TJvDockConjoinPanel read FPanel write FPanel;
  end;

  TJvDockTabHostForm = class(TJvDockableForm)
  private
    FPageControl: TJvDockTabPageControl;
  public
    constructor Create(AOwner: TComponent); override;
    function GetActiveDockForm: TForm;
    property DockClient;
    property PageControl: TJvDockTabPageControl read FPageControl write FPageControl;
  end;

var
  DefaultDockPanelClass: TJvDockPanelClass = TJvDockPanel;
  DefaultDockSplitterClass: TJvDockSplitterClass = TJvDockSplitter;
  DefaultConjoinPanelClass: TJvDockConjoinPanelClass = TJvDockConjoinPanel;
  DefaultTabDockClass: TJvDockTabClass = TJvDockTabPageControl;
  DefaultDockZoneClass: TJvDockZoneClass = TJvDockZone;
  DefaultDockTreeClass: TJvDockTreeClass = TJvDockTree;

procedure ShowDockForm(DockWindow: TWinControl);
procedure HideDockForm(DockWindow: TWinControl);
procedure MakeDockClientEvent(Host: TControl; Visible: Boolean);
function GetFormVisible(DockWindow: TWinControl): Boolean;
procedure SetDockPageControlPopupMenu(Value: TPopupMenu);
procedure SetDockPageControlHotTrack(Value: Boolean);
procedure SetTabDockHostBorderStyle(Value: TFormBorderStyle);
procedure SetConjoinDockHostBorderStyle(Value: TFormBorderStyle);

{$IFDEF USEJVCL}
procedure SaveDockTreeToAppStorage(AppStorage: TJvCustomAppStorage; AppStoragePath: string = '');
procedure LoadDockTreeFromAppStorage(AppStorage: TJvCustomAppStorage; AppStoragePath: string = '');
{$ENDIF USEJVCL}

procedure SaveDockTreeToFile(FileName: string);
procedure LoadDockTreeFromFile(FileName: string);
procedure SaveDockTreeToReg(ARootKey: DWORD; RegPatch: string);
procedure LoadDockTreeFromReg(ARootKey: DWORD; RegPatch: string);

function FindDockBaseControl(Client: TControl): TJvDockBaseControl;
function FindDockClient(Client: TControl): TJvDockClient;
function FindDockServer(Client: TControl): TJvDockServer;

function IsDockable(Sender: TWinControl; Client: TControl;
  DropCtl: TControl = nil; DockAlign: TAlign = alNone): Boolean;
function ComputeDockingRect(AControl: TControl;
  var DockRect: TRect; MousePos: TPoint): TAlign;

procedure DoFloat(Sender, AControl: TControl);
procedure SetDockSite(Control: TWinControl; SiteValue: Boolean);
procedure DoFloatForm(DockForm: TControl);
procedure FreeAllDockableForm;
procedure DoFloatAllForm;

function GetClientAlignControlArea(AControl: TWinControl; Align: TAlign; Exclude: TControl = nil): Integer;
procedure ResetDockClient(Control: TControl; NewTarget: TControl); overload;
procedure ResetDockClient(DockClient: TJvDockClient; NewTarget: TControl); overload;
procedure ReshowAllVisibleWindow;

implementation

uses
  SysUtils,
  {$IFDEF USEJVCL}
  JvAppRegistryStorage, JvAppIniStorage, JvTypes,
  {$ELSE}
  IniFiles, Registry,
  {$ENDIF USEJVCL}
  JvDockSupportProc, JvDockGlobals, JvDockInfo, JvDockVSNetStyle;

{$R JvDockableForm.dfm}
{$R JvDockConjoinHost.dfm}
{$R JvDockTabHost.dfm}

const
  cDefaultFormName = 'A_B_C_D_E_F_G_H_I_J_K_L_M_N';
  cDefaultNameSuffix = '_A_B_C_D_E_F_G';

var
  DockPageControlPopupMenu: TPopupMenu = nil;
  DockPageControlHotTrack: Boolean = False;
  TabDockHostBorderStyle: TFormBorderStyle = bsSizeToolWin;
  ConjoinDockHostBorderStyle: TFormBorderStyle = bsSizeToolWin;
  IsWinXP: Boolean;

type
  TControlAccessProtected = class(TControl);
  TWinControlAccessProtected = class(TWinControl);

function FindDockBaseControl(Client: TControl): TJvDockBaseControl;
var
  I: Integer;
begin
  Result := nil;
  if Client <> nil then
  begin
    for I := 0 to Client.ComponentCount - 1 do
      if Client.Components[I] is TJvDockBaseControl then
      begin
        Result := TJvDockBaseControl(Client.Components[I]);
        Break;
      end;
  end;
end;

function FindDockServer(Client: TControl): TJvDockServer;
var
  ADockControl: TJvDockBaseControl;
begin
  ADockControl := FindDockBaseControl(Client);
  if ADockControl is TJvDockServer then
    Result := TJvDockServer(ADockControl)
  else
    Result := nil;
end;

function FindDockClient(Client: TControl): TJvDockClient;
var
  ADockControl: TJvDockBaseControl;
begin
  ADockControl := FindDockBaseControl(Client);
  if ADockControl is TJvDockClient then
    Result := TJvDockClient(ADockControl)
  else
    Result := nil;
end;

function IsDockable(Sender: TWinControl; Client: TControl; DropCtl: TControl = nil;
  DockAlign: TAlign = alNone): Boolean;
var
  ADockClient: TJvDockClient;
  I: Integer;
  SenderDockStyle,
    ClientDockStyle: TJvDockBasicStyle;
  SenderStyleName,
    ClientStyleName: string;
  // (rom) disabled unused
  //label
  // JudgeRelation;
begin
  ADockClient := FindDockClient(Client);
  Result := False;
  if (ADockClient <> nil) and (ADockClient.EnableDock) then
  begin
    if Sender is TJvDockPanel then
    begin
      with TJvDockPanel(Sender) do
      begin
        Result := DockServer.EnableDock and
          (((Align = alLeft) and DockServer.LeftDock and (ADockClient.LeftDock)) or
          ((Align = alTop) and DockServer.TopDock and (ADockClient.TopDock)) or
          ((Align = alRight) and DockServer.RightDock and (ADockClient.RightDock)) or
          ((Align = alBottom) and DockServer.BottomDock and (ADockClient.BottomDock)));
        SenderDockStyle := DockServer.DockStyle;
      end;
    end
    else
    begin
      if (Sender <> nil) and (Sender.Parent is TJvDockableForm) then
        with TJvDockableForm(Sender.Parent).DockableControl do
          for I := 0 to DockClientCount - 1 do
            if DockClients[I] = Client then
              Exit;
      Result := ADockClient.EachOtherDock;
      if Sender <> nil then
        ADockClient := FindDockClient(Sender.Parent);
      if ADockClient <> nil then
        Result := Result and ADockClient.EachOtherDock;

      if ADockClient <> nil then
        SenderDockStyle := ADockClient.DockStyle
      else
        Exit;
    end;

    ADockClient := FindDockClient(Client);
    if ADockClient <> nil then
      ClientDockStyle := ADockClient.DockStyle
    else
      Exit;

    if SenderDockStyle = nil then
      SenderStyleName := ''
    else
      SenderStyleName := SenderDockStyle.ClassName;

    if ClientDockStyle = nil then
      ClientStyleName := ''
    else
      ClientStyleName := ClientDockStyle.ClassName;

    Result := Result and (SenderStyleName = ClientStyleName);

    //JudgeRelation:
  end;
end;

procedure UpdateCaption(Source: TWinControl; Exclude: TControl);
var
  I: Integer;
  Host: TJvDockableForm;
begin
  if (Source <> nil) and (Source.Parent is TJvDockableForm) then
  begin
    Host := TJvDockableForm(Source.Parent);
    Host.Caption := '';

    for I := 0 to Source.DockClientCount - 1 do
      if Source.DockClients[I].Visible and (Source.DockClients[I] <> Exclude) then
        Host.Caption := Host.Caption + TCustomForm(Source.DockClients[I]).Caption + RsDockStringSplitter;

    if Host.HostDockSite is TJvDockTabPageControl then
      with TJvDockTabPageControl(Host.HostDockSite) do
        if (ActivePage <> nil) and (ActivePage.Controls[0] = Source) then
          ActivePage.Caption := Host.Caption;
    UpdateCaption(Host.HostDockSite, nil);
  end;
end;

procedure DoFloat(Sender, AControl: TControl);
var
  ARect: TRect;
  CH, BW: Integer;
begin
  BW := JvDockGetSysBorderWidth;
  CH := JvDockGetSysCaptionHeight;

  ARect.TopLeft := Sender.ClientToScreen(Point(-(BW + 3), -(CH + BW + 1)));
  ARect.BottomRight := Sender.ClientToScreen(
    Point(Sender.UndockWidth - (BW + 3), Sender.UndockHeight - (BW + CH + 1)));
  AControl.ManualFloat(ARect);
  if (AControl.Left <> ARect.Left) or (AControl.Top <> ARect.Top) then
  begin
    AControl.Left := ARect.Left;
    AControl.Top := ARect.Top;
  end;
end;

function ComputeDockingRect(AControl: TControl; var DockRect: TRect; MousePos: TPoint): TAlign;
var
  DockTopRect, DockLeftRect, DockBottomRect, DockRightRect, DockCenterRect: TRect;
begin
  Result := alNone;

  if AControl = nil then
    Exit;
  with AControl do
  begin
    DockLeftRect.TopLeft := Point(0, 0);
    DockLeftRect.BottomRight := Point(ClientWidth div 5, ClientHeight);

    DockTopRect.TopLeft := Point(ClientWidth div 5, 0);
    DockTopRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight div 5);

    DockRightRect.TopLeft := Point(ClientWidth div 5 * 4, 0);
    DockRightRect.BottomRight := Point(ClientWidth, ClientHeight);

    DockBottomRect.TopLeft := Point(ClientWidth div 5, ClientHeight div 5 * 4);
    DockBottomRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight);

    DockCenterRect.TopLeft := Point(ClientWidth div 5, ClientHeight div 5);
    DockCenterRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight div 5 * 4);

    if PtInRect(DockLeftRect, MousePos) then
    begin
      Result := alLeft;
      DockRect := DockLeftRect;
      DockRect.Right := ClientWidth div 2;
    end
    else
    if PtInRect(DockTopRect, MousePos) then
    begin
      Result := alTop;
      DockRect := DockTopRect;
      DockRect.Left := 0;
      DockRect.Right := ClientWidth;
      DockRect.Bottom := ClientHeight div 2;
    end
    else
    if PtInRect(DockRightRect, MousePos) then
    begin
      Result := alRight;
      DockRect := DockRightRect;
      DockRect.Left := ClientWidth div 2;
    end
    else
    if PtInRect(DockBottomRect, MousePos) then
    begin
      Result := alBottom;
      DockRect := DockBottomRect;
      DockRect.Left := 0;
      DockRect.Right := ClientWidth;
      DockRect.Top := ClientHeight div 2;
    end
    else
    if PtInRect(DockCenterRect, MousePos) then
    begin
      Result := alClient;
      DockRect := DockCenterRect;
    end;
    if Result = alNone then
      Exit;

    DockRect.TopLeft := ClientToScreen(DockRect.TopLeft);
    DockRect.BottomRight := ClientToScreen(DockRect.BottomRight);
  end;
end;

procedure ShowDockForm(DockWindow: TWinControl);
var
  TmpDockWindow: TWinControl;

  procedure ShowClient(Client, DockParent: TWinControl);
  var
    ADockClient: TJvDockClient;
    I: Integer;
  begin
    if (DockParent is TJvDockableForm) and (Client <> nil) then
    begin
      with TJvDockableForm(DockParent).DockableControl do
        for I := 0 to DockClientCount - 1 do
          if DockClients[I] <> Client then
            MakeDockClientEvent(DockClients[I], True);
      if Client.HostDockSite is TJvDockCustomControl then
        TJvDockCustomControl(Client.HostDockSite).UpdateCaption(nil);
    end
    else
    begin
      ADockClient := FindDockClient(DockParent);
      if ADockClient <> nil then
      begin
        ADockClient.DockStyle.ShowDockForm(ADockClient);
        if DockParent.CanFocus then
          DockParent.SetFocus;
      end;
    end;
    if DockParent.Parent = nil then
      SetForegroundWindow(DockParent.Handle);
  end;

  function ShowDockPanel(Client: TWinControl): TWinControl;
  begin
    Result := Client;
    if Client <> nil then
      if Client.HostDockSite is TJvDockPanel then
      begin
        TJvDockPanel(Client.HostDockSite).ShowDockPanel(True, Client, sdfDockPanel);
        Result := nil;
      end;
  end;

  function ShowTabDockHost(Client: TWinControl): TWinControl;
  var
    I: Integer;
  begin
    Result := Client;
    if Client <> nil then
    begin
      ShowClient(nil, Client);
      if Client.HostDockSite is TJvDockTabPageControl then
      begin
        with TJvDockTabPageControl(Client.HostDockSite) do
          for I := 0 to Count - 1 do
            if Pages[I].Controls[0] = Client then
            begin
              Pages[I].Show;
              Break;
            end;
        if (Client.HostDockSite <> nil) and not (Client.HostDockSite is TJvDockPanel) then
        begin
          Result := Client.HostDockSite.Parent;
          ShowClient(Client, Result);
          if (Result <> nil) and (Result.HostDockSite is TJvDockTabPageControl) then
            Result := ShowTabDockHost(Result);
        end;
      end;
    end;
  end;

  function ShowConjoinDockHost(Client: TWinControl): TWinControl;
  begin
    Result := Client;
    if Client <> nil then
    begin
      ShowClient(nil, Client);
      if (Client.HostDockSite <> nil) and not (Client.HostDockSite is TJvDockPanel) then
      begin
        if Client.HostDockSite.Parent <> nil then
        begin
          Result := Client.HostDockSite.Parent;
          ShowClient(Client, Result);
          if (Result <> nil) and (Result.HostDockSite is TJvDockConjoinPanel) then
            Result := ShowConjoinDockHost(Result);
        end;
      end;
    end;
  end;

  procedure ShowPopupPanel(Client: TWinControl);
  begin
    if Client.HostDockSite is TJvDockVSPopupPanel then
      TJvDockVSPopupPanel(Client.HostDockSite).VSChannel.PopupDockForm(Client)
    else
    if (Client.HostDockSite <> nil) and (Client.HostDockSite.Parent <> nil) then
    begin
      if (Client.HostDockSite.Parent.HostDockSite is TJvDockVSPopupPanel) then
        TJvDockVSPopupPanel(Client.HostDockSite.Parent.HostDockSite).VSChannel.PopupDockForm(Client)
      else
      if Client.HostDockSite.Parent.HostDockSite is TJvDockPanel then
        Client.HostDockSite.Parent.HostDockSite.Invalidate;
    end;
  end;

begin
  TmpDockWindow := DockWindow;
  repeat
    DockWindow := ShowTabDockHost(DockWindow);
    DockWindow := ShowConjoinDockHost(DockWindow);
    DockWindow := ShowDockPanel(DockWindow);
  until (DockWindow = nil) or (DockWindow.Parent = nil);
  ShowPopupPanel(TmpDockWindow);
end;

procedure HideDockForm(DockWindow: TWinControl);
var
  TmpDockWindow: TWinControl;

  procedure HideDockChild(DockWindow: TWinControl);
  var
    I: Integer;
    DockClient: TJvDockClient;
  begin
    if DockWindow = nil then
      Exit;
    if (DockWindow is TJvDockableForm) and (DockWindow.Visible) then
      with TJvDockableForm(DockWindow).DockableControl do
        for I := 0 to DockClientCount - 1 do
          HideDockChild(TWinControl(DockClients[I]));
    DockClient := FindDockClient(DockWindow);
    if (DockWindow is TForm) and (TForm(DockWindow).FormStyle <> fsMDIChild) and (DockClient.DockStyle <> nil) then
      DockClient.DockStyle.HideDockForm(DockClient);
  end;

  procedure HideDockParent(DockWindow: TWinControl);
  var
    Host: TWinControl;
    DockClient: TJvDockClient;
  begin
    if (DockWindow <> nil) and (DockWindow.HostDockSite <> nil) then
    begin
      Host := DockWindow.HostDockSite;
      if Host.VisibleDockClientCount = 0 then
        if Host is TJvDockPanel then
          TJvDockPanel(Host).ShowDockPanel(False, nil)
        else
        begin
          if Host.Parent <> nil then
          begin
            DockClient := FindDockClient(Host.Parent);
            if (DockClient <> nil) and (DockClient.DockStyle <> nil) then
              DockClient.DockStyle.HideDockForm(DockClient);
            HideDockParent(Host.Parent);
          end;
        end;
    end;
  end;

  procedure HidePopupPanel(Client: TWinControl);
  begin
    if Client.HostDockSite is TJvDockVSPopupPanel then
      TJvDockVSPopupPanel(Client.HostDockSite).VSChannel.HidePopupPanel(Client)
    else
    if (Client.HostDockSite <> nil) and (Client.HostDockSite.Parent <> nil) then
    begin
      if (Client.HostDockSite.Parent.HostDockSite is TJvDockVSPopupPanel) then
        TJvDockVSPopupPanel(Client.HostDockSite.Parent.HostDockSite).VSChannel.HidePopupPanel(Client)
      else
      if (Client.HostDockSite.Parent.HostDockSite is TJvDockPanel) then
        Client.HostDockSite.Parent.HostDockSite.Invalidate
    end;
  end;

begin
  TmpDockWindow := TWinControl(DockWindow);
  HideDockChild(DockWindow);
  HideDockParent(DockWindow);
  if (DockWindow.HostDockSite is TJvDockCustomControl) then
    TJvDockCustomControl(DockWindow.HostDockSite).UpdateCaption(DockWindow);
  HidePopupPanel(TmpDockWindow);
end;

procedure MakeDockClientEvent(Host: TControl; Visible: Boolean);
var
  I: Integer;
  ADockClient: TJvDockClient;
begin
  ADockClient := FindDockClient(Host);
  if ADockClient <> nil then
  begin
    if Visible then
      ADockClient.MakeShowEvent
    else
      ADockClient.MakeHideEvent;
    if (Host is TJvDockableForm) and Host.Visible then
      with TJvDockableForm(Host).DockableControl do
        for I := 0 to DockClientCount - 1 do
          MakeDockClientEvent(DockClients[I], Visible);
  end;
end;

function GetFormVisible(DockWindow: TWinControl): Boolean;
var
  ADockClient: TJvDockClient;
begin
  Result := True;
  ADockClient := FindDockClient(DockWindow);
  if ADockClient <> nil then
    Result := ADockClient.DockStyle.GetDockFormVisible(ADockClient);
end;

procedure SetDockPageControlPopupMenu(Value: TPopupMenu);
var
  I: Integer;
begin
  DockPageControlPopupMenu := Value;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.CustomForms[I] is TJvDockTabHostForm then
      TJvDockTabHostForm(Screen.CustomForms[I]).PageControl.PopupMenu := Value;
end;

procedure SetDockPageControlHotTrack(Value: Boolean);
var
  I: Integer;
begin
  DockPageControlHotTrack := Value;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.CustomForms[I] is TJvDockTabHostForm then
      TJvDockTabHostForm(Screen.CustomForms[I]).PageControl.HotTrack := Value;
end;

procedure SetTabDockHostBorderStyle(Value: TFormBorderStyle);
var
  I: Integer;
begin
  TabDockHostBorderStyle := Value;
  for I := 0 to Screen.FormCount - 1 do
    if (Screen.CustomForms[I] is TJvDockTabHostForm) and (Screen.CustomForms[I].HostDockSite = nil) then
      TJvDockTabHostForm(Screen.CustomForms[I]).BorderStyle := Value;
end;

procedure SetConjoinDockHostBorderStyle(Value: TFormBorderStyle);
var
  I: Integer;
begin
  ConjoinDockHostBorderStyle := Value;
  for I := 0 to Screen.FormCount - 1 do
    if (Screen.CustomForms[I] is TJvDockConjoinHostForm) and (Screen.CustomForms[I].HostDockSite = nil) then
      TJvDockConjoinHostForm(Screen.CustomForms[I]).BorderStyle := Value;
end;

procedure ReshowAllVisibleWindow;
var
  I: Integer;
begin
  if IsWinXP then
    for I := 0 to Screen.FormCount - 1 do
      if Screen.Forms[I].Visible then
        Windows.ShowWindow(Screen.Forms[I].Handle, SW_SHOW)
      else
        Windows.ShowWindow(Screen.Forms[I].Handle, SW_HIDE);
end;

{$IFDEF USEJVCL}

procedure SaveDockTreeToAppStorage(AppStorage: TJvCustomAppStorage; AppStoragePath: string = '');
var
  JvDockInfoTree: TJvDockInfoTree;
  I: Integer;
begin
  AppStorage.BeginUpdate;
  try
    HideAllPopupPanel(nil);
    JvDockInfoTree := TJvDockInfoTree.Create(TJvDockInfoZone);
    try
      for I := 0 to Screen.CustomFormCount - 1 do
        if (Screen.CustomForms[I].Parent = nil) and
          ((FindDockClient(Screen.CustomForms[I]) <> nil) or (FindDockServer(Screen.CustomForms[I]) <> nil)) then
          JvDockInfoTree.CreateZoneAndAddInfoFromApp(Screen.CustomForms[I]);

      JvDockInfoTree.AppStorage := AppStorage;
      if AppStoragePath <> '' then
        JvDockInfoTree.AppStoragePath := AppStoragePath
      else
        JvDockInfoTree.AppStoragePath := AppStorage.Path;
      JvDockInfoTree.WriteInfoToAppStorage;
    finally
      JvDockInfoTree.Free;
    end;
  finally
    AppStorage.EndUpdate;
  end;
end;

procedure LoadDockTreeFromAppStorage(AppStorage: TJvCustomAppStorage; AppStoragePath: string = '');
var
  JvDockInfoTree: TJvDockInfoTree;
  Form: TForm;
begin
  AppStorage.BeginUpdate;
  try
    HideAllPopupPanel(nil);

    Form := TForm.CreateNew(nil);
    Form.BorderStyle := bsNone;
    Form.BoundsRect := Rect(0, 0, 0, 0);
    Form.Visible := True;
    Form.Name := cDefaultFormName;

    JvDockInfoTree := TJvDockInfoTree.Create(TJvDockInfoZone);

    JvDockLockWindow(nil);
    try
      JvDockInfoTree.AppStorage := AppStorage;
      if AppStoragePath <> '' then
        JvDockInfoTree.AppStoragePath := AppStoragePath
      else
        JvDockInfoTree.AppStoragePath := AppStorage.Path;
      try
        JvGlobalDockIsLoading := True;
        JvDockInfoTree.ReadInfoFromAppStorage;
      finally
        JvGlobalDockIsLoading := False;
      end;
    finally
      Form.Release;
      JvDockUnLockWindow;
      JvDockInfoTree.Free;
    end;
    ReshowAllVisibleWindow;
  finally
    AppStorage.EndUpdate;
  end;
end;

procedure SaveDockTreeToFile(FileName: string);
var
  JvAppStorage: TJvAppIniFileStorage;
begin
  JvAppStorage := TJvAppIniFileStorage.Create(nil);
  try
    JvAppStorage.Location := flCustom;
    JvAppStorage.FileName := FileName;
    JvAppStorage.Reload;
    SaveDockTreeToAppStorage(JvAppStorage);
  finally
    JvAppStorage.Flush;
    JvAppStorage.Free;
  end;
end;

procedure LoadDockTreeFromFile(FileName: string);
var
  JvAppStorage: TJvAppIniFileStorage;
begin
  JvAppStorage := TJvAppIniFileStorage.Create(nil);
  try
    JvAppStorage.Location := flCustom;
    JvAppStorage.FileName := FileName;
    JvAppStorage.Reload;
    LoadDockTreeFromAppStorage(JvAppStorage);
  finally
    JvAppStorage.Free;
  end;
end;

procedure SaveDockTreeToReg(ARootKey: DWORD; RegPatch: string);
var
  JvAppStorage: TJvAppRegistryStorage;
begin
  JvAppStorage := TJvAppRegistryStorage.Create(nil);
  try
    // (p3) this seems dangerous but it's the same method as used by TJvAppRegistryStorage
    JvAppStorage.RegRoot := TJvRegKey(HKEY_CLASSES_ROOT + ARootKey);
    JvAppStorage.Path := RegPatch;
    SaveDockTreeToAppStorage(JvAppStorage);
  finally
    JvAppStorage.Free;
  end;
end;

procedure LoadDockTreeFromReg(ARootKey: DWORD; RegPatch: string);
var
  JvAppStorage: TJvAppRegistryStorage;
begin
  JvAppStorage := TJvAppRegistryStorage.Create(nil);
  try
    // (p3) this seems dangerous but it's the same method as used by TJvAppRegistryStorage
    JvAppStorage.RegRoot := TJvRegKey(HKEY_CLASSES_ROOT + ARootKey);
    JvAppStorage.Path := RegPatch;
    LoadDockTreeFromAppStorage(JvAppStorage);
  finally
    JvAppStorage.Free;
  end;
end;

{$ELSE}

procedure SaveDockTreeToFile(FileName: string);
var
  JvDockInfoTree: TJvDockInfoTree;
  I: Integer;
  MemFile: TMemIniFile;
begin
  HideAllPopupPanel(nil);
  MemFile := TMemIniFile.Create(Filename);
  try
    JvDockInfoTree := TJvDockInfoTree.Create(TJvDockInfoZone);
    try
      for I := 0 to Screen.CustomFormCount - 1 do
        if (Screen.CustomForms[I].Parent = nil) and
          ((FindDockClient(Screen.CustomForms[I]) <> nil) or (FindDockServer(Screen.CustomForms[I]) <> nil)) then
          JvDockInfoTree.CreateZoneAndAddInfoFromApp(Screen.CustomForms[I]);

      JvDockInfoTree.DockInfoIni := MemFile;
      JvDockInfoTree.WriteInfoToIni;
      MemFile.UpdateFile;
    finally
      JvDockInfoTree.Free;
    end;
  finally
    MemFile.Free;
  end;
end;

procedure LoadDockTreeFromFile(FileName: string);
var
  JvDockInfoTree: TJvDockInfoTree;
  Form: TForm;
  MemFile: TMemIniFile;
begin
  HideAllPopupPanel(nil);

  Form := TForm.CreateNew(nil);
  Form.BorderStyle := bsNone;
  Form.BoundsRect := Rect(0, 0, 0, 0);
  Form.Visible := True;
  Form.Name := cDefaultFormName;

  JvDockInfoTree := TJvDockInfoTree.Create(TJvDockInfoZone);

  MemFile := TMemIniFile.Create(FileName);
  JvDockLockWindow(nil);
  try
    JvDockInfoTree.DockInfoIni := MemFile;
    JvGlobalDockIsLoading := True;
    JvDockInfoTree.ReadInfoFromIni;
    JvGlobalDockIsLoading := False;
  finally
    Form.Release;
    JvDockUnLockWindow;
    JvDockInfoTree.Free;
    MemFile.Free;
  end;
  ReshowAllVisibleWindow;
end;

procedure SaveDockTreeToReg(ARootKey: DWORD; RegPatch: string);
var
  JvDockInfoTree: TJvDockInfoTree;
  I: Integer;
begin
  HideAllPopupPanel(nil);

  JvDockInfoTree := TJvDockInfoTree.Create(TJvDockInfoZone);
  try
    for I := 0 to Screen.CustomFormCount - 1 do
      if (Screen.CustomForms[I].Parent = nil) and
        (FindDockClient(Screen.CustomForms[I]) <> nil) then
        JvDockInfoTree.CreateZoneAndAddInfoFromApp(Screen.CustomForms[I]);

    for I := 0 to Screen.CustomFormCount - 1 do
      if (Screen.CustomForms[I].Parent = nil) and
        (FindDockServer(Screen.CustomForms[I]) <> nil) then
        JvDockInfoTree.CreateZoneAndAddInfoFromApp(Screen.CustomForms[I]);

    JvDockInfoTree.DockInfoReg := TRegistry.Create;
    try
      JvDockInfoTree.DockInfoReg.RootKey := ARootKey;
      JvDockInfoTree.WriteInfoToReg(RegPatch);
    finally
      JvDockInfoTree.DockInfoReg.Free;
    end;
  finally
    JvDockInfoTree.Free;
  end;
end;

procedure LoadDockTreeFromReg(ARootKey: DWORD; RegPatch: string);
var
  JvDockInfoTree: TJvDockInfoTree;
  Form: TForm;
begin
  HideAllPopupPanel(nil);

  Form := TForm.CreateNew(nil);
  Form.BorderStyle := bsNone;
  Form.BoundsRect := Rect(0, 0, 0, 0);
  Form.Visible := True;
  Form.Name := cDefaultFormName;

  JvDockInfoTree := TJvDockInfoTree.Create(TJvDockInfoZone);

  JvDockLockWindow(nil);
  try
    JvDockInfoTree.DockInfoReg := TRegistry.Create;
    try
      JvGlobalDockIsLoading := True;
      JvDockInfoTree.DockInfoReg.RootKey := ARootKey;
      JvDockInfoTree.ReadInfoFromReg(RegPatch);
    finally
      JvDockInfoTree.DockInfoReg.Free;
      JvGlobalDockIsLoading := False;
    end;
  finally
    JvDockUnLockWindow;
    JvDockInfoTree.Free;
    Form.Release;
  end;
  ReshowAllVisibleWindow;
end;

{$ENDIF USEJVCL}

procedure SetDockSite(Control: TWinControl; SiteValue: Boolean);
begin
  TWinControlAccessProtected(Control).DockSite := SiteValue;
  if (not (csDesigning in Control.ComponentState)) and (JvGlobalDockManager <> nil) then
    JvGlobalDockManager.RegisterDockSite(Control, SiteValue);
end;

procedure DoFloatForm(DockForm: TControl);
var
  I: TAlign;
  J: Integer;
  ADockServer: TJvDockServer;
  ARect: TRect;
begin
  if DockForm is TJvDockableForm then
  begin
    with TJvDockableForm(DockForm).DockableControl do
    begin
      for J := DockClientCount - 1 downto 0 do
        DoFloatForm(DockClients[J]);

      DockForm.ManualDock(nil);
    end;
  end
  else
  begin
    ADockServer := FindDockServer(DockForm);
    if ADockServer <> nil then
    begin
      // (rom) better use a Count or introduce one
      // (p3) this is due to the fact that DockPanel returns a dockpanel based on the indices 0 to 3
      //  DockPanelWithAlign uses the TAlign enumeration, however
      for I := alTop to alRight do
      begin
        for J := ADockServer.DockPanelWithAlign[I].DockClientCount - 1 downto 0 do
          DoFloatForm(ADockServer.DockPanelWithAlign[I].DockClients[J]);
        if ADockServer.DockPanelWithAlign[I] is TJvDockVSNETPanel then
          with TJvDockVSNETPanel(ADockServer.DockPanelWithAlign[I]).VSChannel do
          begin
            RemoveAllBlock;
            HidePopupPanel(ActiveDockForm);
          end;
      end;
    end
    else
    begin
      if DockForm.HostDockSite <> nil then
      begin
        if (DockForm.HostDockSite.Parent is TJvDockableForm) and
          (DockForm.HostDockSite.DockClientCount <= 2) then
          PostMessage(DockForm.HostDockSite.Parent.Handle, WM_CLOSE, 0, 0);
      end
      else
        ARect := DockForm.BoundsRect;

      if DockForm.HostDockSite is TJvDockVSPopupPanel then
      begin
        TJvDockVSPopupPanel(DockForm.HostDockSite).VSNETDockPanel.VSChannel.RemoveDockControl(TWinControl(DockForm));
        DockForm.Dock(nil, Bounds(DockForm.Left, DockForm.Top, DockForm.UndockWidth, DockForm.UndockHeight));
      end
      else
        DockForm.ManualDock(nil);
    end;
  end;
end;

procedure FreeAllDockableForm;
var
  I: Integer;
begin
  Assert(JvGlobalDockManager <> nil);
  for I := JvGlobalDockManager.DockableFormList.Count - 1 downto 0 do
    if TJvDockableForm(JvGlobalDockManager.DockableFormList[I]).DockableControl.DockClientCount = 0 then
      TJvDockableForm(JvGlobalDockManager.DockableFormList[I]).Free;
end;

procedure DoFloatAllForm;
var
  I: Integer;
  TempList: TList;
begin
  TempList := TList.Create;
  try
    for I := 0 to Screen.CustomFormCount - 1 do
      if not (Screen.CustomForms[I] is TJvDockableForm) then
        TempList.Add(Screen.CustomForms[I]);

    for I := 0 to TempList.Count - 1 do
      DoFloatForm(TempList[I]);
  finally
    TempList.Free;
  end;
  FreeAllDockableForm;
end;

function GetClientAlignControlArea(AControl: TWinControl; Align: TAlign; Exclude: TControl): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AControl.ControlCount - 1 do
    if (AControl.Controls[I].Align = Align) and AControl.Controls[I].Visible and (AControl.Controls[I] <> Exclude) and
      not ((AControl.Controls[I] is TJvDockSplitter) or (AControl.Controls[I] is TJvDockPanel)) then
      if Align in [alLeft, alRight] then
        Inc(Result, AControl.Controls[I].Width)
      else
        Inc(Result, AControl.Controls[I].Height);
end;

function GetActiveControl(AForm: TCustomForm): TWinControl;
var
  AControl: TWinControl;
begin
  Result := nil;
  AControl := AForm.ActiveControl;
  while AControl <> nil do
  begin
    if AControl.HostDockSite <> nil then
    begin
      Result := AControl;
      Break;
    end;
    AControl := AControl.Parent;
  end;
end;

function GetHostDockParent(AControl: TWinControl): TWinControl;
begin
  Result := nil;
  while AControl <> nil do
  begin
    if AControl.HostDockSite <> nil then
    begin
      Result := AControl.HostDockSite;
      Break;
    end;
    AControl := AControl.Parent;
  end;
end;

procedure ResetDockClient(Control: TControl; NewTarget: TControl);
begin
  ResetDockClient(FindDockClient(Control), NewTarget);
end;

procedure ResetDockClient(DockClient: TJvDockClient; NewTarget: TControl);
var
  Pt: TPoint;
begin
  if DockClient <> nil then
  begin
    if (DockClient.ParentForm.HostDockSite is TJvDockPanel) and (NewTarget is TJvDockPanel) then
    begin
    end
    else
    begin
      if (DockClient.LastDockSite is TJvDockPanel) and (NewTarget is TJvDockPanel) and
        (DockClient.LastDockSite <> NewTarget) then
        with TJvDockPanel(DockClient.LastDockSite) do
          if UseDockManager and (JvDockManager <> nil) then
            JvDockManager.RemoveControl(DockClient.ParentForm);

      if DockClient.ParentForm.HostDockSite is TJvDockPanel then
        DockClient.LastDockSite := DockClient.ParentForm.HostDockSite
      else
        DockClient.LastDockSite := nil;

      if DockClient.ParentForm.HostDockSite = nil then
      begin
        DockClient.UnDockLeft := DockClient.ParentForm.BoundsRect.TopLeft.X;
        DockClient.UnDockTop := DockClient.ParentForm.BoundsRect.TopLeft.Y;
      end
      else
      begin
        Pt := DockClient.ParentForm.BoundsRect.TopLeft;
        Pt := DockClient.ParentForm.HostDockSite.ClientToScreen(Pt);
        DockClient.UnDockLeft := Pt.X;
        DockClient.UnDockTop := Pt.Y;
      end;
    end;
  end;
end;

//=== { TJvDockPanel } =======================================================

constructor TJvDockPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  BevelOuter := bvNone;
  Width := 10;
  Height := 10;
end;

procedure TJvDockPanel.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  inherited CustomDockDrop(Source, X, Y);

  if Source.Control <> nil then
    ShowDockPanel(True, Source.Control);
end;

procedure TJvDockPanel.CustomDockOver(Source: TJvDockDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
  if Accept then
    inherited CustomDockOver(Source, X, Y, State, Accept);
end;

procedure TJvDockPanel.CustomEndDock(Target: TObject; X, Y: Integer);
begin
  inherited CustomEndDock(Target, X, Y);
end;

procedure TJvDockPanel.CustomGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock);
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
end;

procedure TJvDockPanel.CustomPositionDockRect(Source: TJvDockDragDockObject; X, Y: Integer);
var
  ARect: TRect;
begin
  inherited CustomPositionDockRect(Source, X, Y);
  if VisibleDockClientCount = 0 then
    if JvGlobalDockClient <> nil then
    begin
      case Align of
        alTop:
          begin
            ARect.TopLeft := ClientToScreen(Point(0, 0));
            ARect.BottomRight := ClientToScreen(
              Point(Width, Source.Control.TBDockHeight));
          end;
        alBottom:
          begin
            ARect.TopLeft := ClientToScreen(
              Point(0, -Source.Control.TBDockHeight));
            ARect.BottomRight := ClientToScreen(
              Point(Width, 0));
          end;
        alLeft:
          begin
            ARect.TopLeft := ClientToScreen(Point(0, 0));
            ARect.BottomRight := ClientToScreen(
              Point(Source.Control.LRDockWidth, Height));
          end;
        alRight:
          begin
            ARect.TopLeft := ClientToScreen(Point(-Source.Control.LRDockWidth, 0));
            ARect.BottomRight := ClientToScreen(Point(Width, Height));
          end;
      end;
      Source.DockRect := ARect;
    end;
end;

procedure TJvDockPanel.CustomStartDock(var Source: TJvDockDragDockObject);
begin
  inherited CustomStartDock(Source);
end;

function TJvDockPanel.CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  ShowDockPanel(False, nil);
  Result := inherited CustomUnDock(Source, NewTarget, Client);
  if not (Client is TJvDockableForm) then
    SetDockSite(TForm(Client), True);
end;

procedure TJvDockPanel.ReloadDockedControl(const AControlName: string;
  var AControl: TControl);
begin
  AControl := JvDockFindDockFormWithName(AControlName);
end;

procedure TJvDockPanel.ShowDockPanel(MakeVisible: Boolean;
  Client: TControl; PanelSizeFrom: TJvDockSetDockPanelSizeFrom);
const
  DefaultDockSize = 100;
var
  DockHeight, DockWidth: Integer;
begin
  if (not MakeVisible and (VisibleDockClientCount > 1)) or
    (JvGlobalDockClient = nil) then
    Exit;

  DockServer.DockSplitter[Integer(Align) - 1].Visible := MakeVisible;

  if MakeVisible and (Client <> nil) then
  begin
    if Width * Height = 0 then
    begin
      if (PanelSizeFrom = sdfDockPanel) or (Client = nil) then
      begin
        DockHeight := TBDockHeight;
        DockWidth := LRDockWidth;
      end
      else
      begin
        DockHeight := Client.TBDockHeight;
        DockWidth := Client.LRDockWidth;
      end;

      if DockHeight = 0 then
        DockHeight := DefaultDockSize;
      if DockWidth = 0 then
        DockWidth := DefaultDockSize;

      Parent.DisableAlign;
      try
        case Align of
          alTop:
            begin
              Top := DockServer.GetClientAlignControl(alTop);
              Height := DockHeight;
              DockServer.TopSplitter.Top := Top + Height;
            end;
          alBottom:
            begin
              Top := Parent.ClientHeight - DockServer.GetClientAlignControl(alBottom) - DockHeight + 1;
              Height := DockHeight;
              DockServer.BottomSplitter.Top := Top + DockServer.BottomSplitter.Height;
            end;
          alLeft:
            begin
              Left := DockServer.GetClientAlignControl(alLeft);
              Width := DockWidth;
              DockServer.LeftSplitter.Left := Left + Width;
            end;
          alRight:
            begin
              Width := DockWidth;
              Left := Parent.ClientWidth - DockServer.GetClientAlignControl(alRight) - DockWidth + 1;
              DockServer.RightSplitter.Left := Left - DockServer.RightSplitter.Width;
            end;
        end;
      finally
        Parent.EnableAlign;
        if UseDockManager and (JvDockManager <> nil) then
          JvDockManager.ResetBounds(True);
      end;
      DockServer.DoFinishSetDockPanelSize(Self);
    end;
  end
  else
  begin
    if (PanelSizeFrom = sdfDockPanel) or (Client = nil) then
    begin
      if Height > 0 then
        TBDockHeight := Height;
      if Width > 0 then
        LRDockWidth := Width;
    end
    else
    begin
      if Height > 0 then
        Client.TBDockHeight := Height;
      if Width > 0 then
        Client.LRDockWidth := Width;
    end;
    if Align in [alLeft, alRight] then
      Width := 0
    else
      Height := 0;

    ResetPosition;
  end;

  if MakeVisible and (Client <> nil) then
  begin
    if not Client.Visible then
      Client.Show;
    if (not TWinControl(Client).Focused) and (TWinControl(Client).CanFocus) then
      TWinControl(Client).SetFocus;
  end;
end;

function TJvDockPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  Result := IsDockable(Self, Client);
  ShowDockPanel(False, nil);
  Result := Result and (Perform(CM_UNDOCKCLIENT, Integer(NewTarget), Integer(Client)) = 0);
  if Result then
    if not (Client is TJvDockableForm) then
      SetDockSite(TForm(Client), True);
end;

procedure TJvDockPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  if Perform(CM_DOCKCLIENT, Integer(Source), Integer(SmallPoint(X, Y))) >= 0 then
  begin
    if Source.Control is TForm then
    begin
      TForm(Source.Control).ActiveControl := nil;
      SetDockSite(TForm(Source.Control), False);
    end;
    UpdateCaption(nil);
  end;
  ShowDockPanel(TWinControl(Source.DragTarget).VisibleDockClientCount > 0, Source.Control);
end;

function TJvDockPanel.GetPanelIndex: Integer;
begin
  case Align of
    alTop:
      Result := 0;
    alBottom:
      Result := 1;
    alLeft:
      Result := 2;
    alRight:
      Result := 3;
  else
    Result := -1;
  end;
end;

procedure TJvDockPanel.Resize;
begin
  inherited Resize;
end;

procedure TJvDockPanel.SetDockServer(const Value: TJvDockServer);
begin
  FDockServer := Value;
end;

procedure TJvDockPanel.ResetPosition;
begin
  case Align of
    alLeft:
      Left := GetClientAlignControlArea(Parent, Align) + 1;
    alRight:
      Left := Parent.ClientWidth - GetClientAlignControlArea(Parent, Align) - Width - 1;
    alTop:
      Top := GetClientAlignControlArea(Parent, Align) + 1;
    alBottom:
      Top := Parent.ClientHeight - GetClientAlignControlArea(Parent, Align) - Height - 1;
  end;
end;

//=== { TJvDockAdvPanel } ====================================================

procedure TJvDockAdvPanel.CMUnDockClient(var Msg: TCMUnDockClient);
var
  DockClient: TJvDockClient;
begin
  if JvGlobalDockIsLoading then
    Exit;
  with Msg do
  begin
    Result := 0;
    if UseDockManager and (JvDockManager <> nil) then
    begin
      DockClient := FindDockClient(Client);
      if (NewTarget <> nil) or
        ((Client <> nil) and (csDestroying in Client.ComponentState)) then
      begin
        if DockClient <> nil then
          DockClient.LastDockSite := nil;
        JvDockManager.RemoveControl(Client);
      end
      else
      begin
        if DockClient <> nil then
          DockClient.LastDockSite := Self;
        JvDockManager.HideControl(Client);
      end;
    end;
  end;
end;

procedure TJvDockAdvPanel.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, Source.TargetControl);
  inherited CustomDockDrop(Source, X, Y);
end;

function TJvDockAdvPanel.CustomUnDock(Source: TJvDockDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, NewTarget);
  Result := inherited CustomUnDock(Source, NewTarget, Client)
end;

procedure TJvDockAdvPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, TControl(Source.DragTarget));
  inherited DockDrop(Source, X, Y);
end;

function TJvDockAdvPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Client, NewTarget);
  Result := inherited DoUnDock(NewTarget, Client);
end;

//=== { TJvDockSplitterStyle } ===============================================

constructor TJvDockSplitterStyle.Create(ASplitter: TJvDockSplitter; ACursor: TCursor);
begin
  inherited Create;
  FSplitter := ASplitter;
  Color := clBtnFace;
  Cursor := ACursor;
  ParentColor := False;
  ResizeStyle := rsPattern;
  FSize := 3;
  FMinSize := 30;
end;

procedure TJvDockSplitterStyle.Assign(Source: TPersistent);
begin
  if Source is TJvDockSplitterStyle then
  begin
    Color := TJvDockSplitterStyle(Source).Color;
    Cursor := TJvDockSplitterStyle(Source).Cursor;
    ParentColor := TJvDockSplitterStyle(Source).ParentColor;
    ResizeStyle := TJvDockSplitterStyle(Source).ResizeStyle;
    Size := TJvDockSplitterStyle(Source).Size;
  end
  else
    inherited Assign(Source);
end;

procedure TJvDockSplitterStyle.AssignTo(Dest: TPersistent);
begin
  if Dest is TJvDockSplitterStyle then
    with TJvDockSplitterStyle(Dest) do
    begin
      Color := Self.Color;
      Cursor := Self.Cursor;
      ParentColor := Self.ParentColor;
      ResizeStyle := Self.ResizeStyle;
      Size := Self.Size;
    end
  else
    inherited AssignTo(Dest);
end;

procedure TJvDockSplitterStyle.AssignToSplitter(Dest: TJvDockSplitter);
begin
  Dest.Color := Color;
  Dest.Cursor := Cursor;
  Dest.ParentColor := ParentColor;
  Dest.ResizeStyle := ResizeStyle;
  if Dest.Align in [alTop, alBottom] then
    Dest.Height := Size
  else
    Dest.Width := Size;
end;

procedure TJvDockSplitterStyle.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    ParentColor := False;
    if Assigned(FSplitter) then
      FSplitter.Color := Value;
  end;
end;

procedure TJvDockSplitterStyle.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
  if Assigned(FSplitter) then
    FSplitter.Cursor := Value;
end;

procedure TJvDockSplitterStyle.SetMinSize(const Value: TJvDockSplitterSize);
begin
  FMinSize := Value;
  if Assigned(FSplitter) then
    FSplitter.MinSize := Value;
end;

procedure TJvDockSplitterStyle.SetParentColor(const Value: Boolean);
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    if Value then
      FColor := FDockServer.ParentForm.Color;
    if Assigned(FSplitter) then
      FSplitter.ParentColor := Value;
  end;
end;

procedure TJvDockSplitterStyle.SetResizeStyle(const Value: TResizeStyle);
begin
  FResizeStyle := Value;
  if Assigned(FSplitter) then
    FSplitter.ResizeStyle := Value;
end;

procedure TJvDockSplitterStyle.SetSize(const Value: TJvDockSplitterSize);
begin
  FSize := Value;
  if Assigned(FSplitter) then
  begin
    if FSplitter.Align in [alTop, alBottom] then
      FSplitter.Height := Value
    else
      FSplitter.Width := Value;
  end;
end;

procedure TJvDockSplitterStyle.SetSplitterStyle;
begin
  AssignToSplitter(FSplitter);
end;

//=== { TJvDockBaseControl } =================================================

constructor TJvDockBaseControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParentForm := TForm(AOwner);
  FEnableDock := True;
  FLeftDock := True;
  FTopDock := True;
  FRightDock := True;
  FBottomDock := True;
  FEachOtherDock := True;
  FDockStyle := nil;
  if not (csDesigning in ComponentState) then
  begin
    FOldOnClose := FParentForm.OnClose;
    ParentForm.OnClose := DoFormOnClose;
    FOldOnCreate := FParentForm.OnCreate;
    ParentForm.OnCreate := DoFormOnCreate;

    FOldWindowProc := FParentForm.WindowProc;

    FParentForm.WindowProc := WindowProc;
  end;
end;

destructor TJvDockBaseControl.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FOldWindowProc) then
      FParentForm.WindowProc := FOldWindowProc;
    FOldWindowProc := nil;
    if Assigned(FDockStyle) and not (FDockStyle is TJvDockBasicStyle) then
      FDockStyle.SetDockBaseControl(False, Self);
  end;
  if FDockStyle <> nil then
    FDockStyle.RemoveDockBaseControl(Self);
  inherited Destroy;
end;

procedure TJvDockBaseControl.Loaded;
begin
  if not (csDesigning in ComponentState) then
  begin
    if not Assigned(DockStyle) then
      DockStyle := TJvDockBasicStyle.Create(ParentForm);
    if Assigned(DockStyle) then
      DockStyle.SetDockBaseControl(True, Self);
  end;
  inherited Loaded;
end;

procedure TJvDockBaseControl.SetBottomDock(const Value: Boolean);
begin
  if CanSetBottomDocked then
    FBottomDock := Value;
end;

procedure TJvDockBaseControl.SetEachOtherDock(const Value: Boolean);
begin
  if CanSetEachOtherDocked then
    FEachOtherDock := Value;
end;

procedure TJvDockBaseControl.SetEnableDock(const Value: Boolean);
begin
  if CanSetEnableDocked then
    FEnableDock := Value;
end;

procedure TJvDockBaseControl.SetLeftDock(const Value: Boolean);
begin
  if CanSetLeftDocked then
    FLeftDock := Value;
end;

procedure TJvDockBaseControl.SetRightDock(const Value: Boolean);
begin
  if CanSetRightDocked then
    FRightDock := Value;
end;

procedure TJvDockBaseControl.SetTopDock(const Value: Boolean);
begin
  if CanSetTopDocked then
    FTopDock := Value;
end;

procedure TJvDockBaseControl.Assign(Source: TPersistent);
begin
  if Source is TJvDockBaseControl then
  begin
    FEnableDock := TJvDockBaseControl(Source).EnableDock;
    FLeftDock := TJvDockBaseControl(Source).LeftDock;
    FTopDock := TJvDockBaseControl(Source).TopDock;
    FRightDock := TJvDockBaseControl(Source).RightDock;
    FBottomDock := TJvDockBaseControl(Source).BottomDock;
    FEachOtherDock := TJvDockBaseControl(Source).EachOtherDock;
    FDockStyle := TJvDockBaseControl(Source).DockStyle;
    if FDockStyle <> nil then
      FDockStyle.AddDockBaseControl(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TJvDockBaseControl.SetDockStyle(const Value: TJvDockBasicStyle);
begin
  if Value = FDockStyle then
    Exit;
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
  begin
    if FDockStyle <> nil then
      FDockStyle.RemoveDockBaseControl(Self);
    if Value <> nil then
    begin
      Value.AddDockBaseControl(Self);
      Value.SetDockBaseControl(True, Self);
      Value.FreeNotification(ParentForm);
    end;
    FDockStyle := Value;
  end
  else
    raise Exception.CreateRes(@RsEDockCannotChangeDockStyleProperty);
end;

procedure TJvDockBaseControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FDockStyle then
    begin
      FDockStyle.SetDockBaseControl(False, Self);
      FDockStyle := nil;
    end;
end;

{$IFNDEF USEJVCL}

function TJvDockBaseControl.GetDockStyleVersion: string;
begin
  Result := RsDockManagerVersion;
end;

{$ENDIF USEJVCL}

procedure TJvDockBaseControl.DoFormOnClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if (Action = caFree) and (JvGlobalDockManager <> nil) then
    if Self is TJvDockServer then
      JvGlobalDockManager.RemoveDockServerFromDockManager(ParentForm)
    else
    if Self is TJvDockClient then
      JvGlobalDockManager.RemoveDockClientFromDockManager(ParentForm);
  if Assigned(FOldOnClose) then
    FOldOnClose(Sender, Action);
end;

procedure TJvDockBaseControl.DoFormOnCreate(Sender: TObject);
begin
  if JvGlobalDockManager <> nil then
    if Self is TJvDockServer then
      JvGlobalDockManager.AddDockServerToDockManager(ParentForm)
    else
    if Self is TJvDockClient then
      JvGlobalDockManager.AddDockClientToDockManager(ParentForm);
  if Assigned(FOldOnCreate) then
    FOldOnCreate(Sender);
end;

procedure TJvDockBaseControl.WindowProc(var Msg: TMessage);
begin
  if not (csDesigning in ComponentState) then
    if Assigned(FOldWindowProc) then
      FOldWindowProc(Msg);
end;

procedure TJvDockBaseControl.SetParentComponent(Value: TComponent);
var
  DockBaseControl: TJvDockBaseControl;
begin
  DockBaseControl := FindDockBaseControl(ParentForm);
  if Assigned(DockBaseControl) and (DockBaseControl <> Self) then
    raise EInvalidOperation.CreateResFmt(@RsEDockCannotLayAnother, [DockBaseControl.ClassName, ClassName]);
  inherited SetParentComponent(Value);
end;

function TJvDockBaseControl.CanSetBottomDocked: Boolean;
begin
  if DockStyle <> nil then
    Result := DockStyle.CanSetBottomDocked(Self)
  else
    Result := True;
end;

function TJvDockBaseControl.CanSetEachOtherDocked: Boolean;
begin
  if DockStyle <> nil then
    Result := DockStyle.CanSetEachOtherDocked(Self)
  else
    Result := True;
end;

function TJvDockBaseControl.CanSetEnableDocked: Boolean;
begin
  if DockStyle <> nil then
    Result := DockStyle.CanSetEnableDocked(Self)
  else
    Result := True;
end;

function TJvDockBaseControl.CanSetLeftDocked: Boolean;
begin
  if DockStyle <> nil then
    Result := DockStyle.CanSetLeftDocked(Self)
  else
    Result := True;
end;

function TJvDockBaseControl.CanSetRightDocked: Boolean;
begin
  if DockStyle <> nil then
    Result := DockStyle.CanSetRightDocked(Self)
  else
    Result := True;
end;

function TJvDockBaseControl.CanSetTopDocked: Boolean;
begin
  if DockStyle <> nil then
    Result := DockStyle.CanSetTopDocked(Self)
  else
    Result := True;
end;

//=== { TJvDockServer } ======================================================

constructor TJvDockServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoFocusDockedForm := True;
  CreateSplitterStyle;
end;

destructor TJvDockServer.Destroy;
begin
  DestroySplitterStyle;
  inherited Destroy;
end;

procedure TJvDockServer.Loaded;
begin
  if Assigned(DockStyle) and Assigned(DockStyle.DockPanelClass) then
    FDockPanelClass := DockStyle.DockPanelClass
  else
    FDockPanelClass := DefaultDockPanelClass;

  if Assigned(DockStyle) and Assigned(DockStyle.DockSplitterClass) then
    FDockSplitterClass := DockStyle.DockSplitterClass
  else
    FDockSplitterClass := DefaultDockSplitterClass;

  if not (csDesigning in ComponentState) then
  begin
    CreateDockPanelAndSplitter;
    SetSplitterStyle;
  end;
  inherited Loaded;
end;

procedure TJvDockServer.CreateDockPanelAndSplitter;
var
  ControlList: TList;

  function CreatePanel(Align: TAlign; Name: string): TJvDockPanel;
  begin
    if (FDockPanelClass <> nil) and
      (FDockPanelClass <> TJvDockPanelClass(ClassType)) then
    begin
      Result := FDockPanelClass.Create(Owner);
      Result.Parent := ParentForm;
      Result.Name := Name;
      Result.Caption := '';
      Result.Align := Align;
      Result.DockServer := Self;
      Result.ResetPosition;
      if Align in [alTop, alBottom] then
        Result.Height := 0
      else
        Result.Width := 0;
      SetDockSite(Result, True);

      if DockStyle <> nil then
        DockStyle.AssignConjoinServerOption(Result);
    end
    else
      Result := nil;
  end;

  function CreateSplitter(Align: TAlign; Name: string): TJvDockSplitter;
  begin
    if (FDockSplitterClass <> nil) and
      (FDockSplitterClass <> TJvDockSplitterClass(ClassType)) then
    begin
      Result := FDockSplitterClass.Create(Owner);
      Result.Parent := ParentForm;
      Result.Name := Name;
      Result.Visible := False;
      Result.Align := Align;
      Result.DockServer := Self;
    end
    else
      Result := nil;
  end;

begin
  ControlList := TList.Create;
  try
    FLeftDockPanel := CreatePanel(alLeft, 'LeftDockPanel' + cDefaultNameSuffix);
    FLeftSplitter := CreateSplitter(alLeft, 'LeftSplitter' + cDefaultNameSuffix);
    FRightDockPanel := CreatePanel(alRight, 'RightDockPanel' + cDefaultNameSuffix);
    FRightSplitter := CreateSplitter(alRight, 'RightSplitter' + cDefaultNameSuffix);
    FTopDockPanel := CreatePanel(alTop, 'TopDockPanel' + cDefaultNameSuffix);
    FTopSplitter := CreateSplitter(alTop, 'TopSplitter' + cDefaultNameSuffix);
    FBottomDockPanel := CreatePanel(alBottom, 'BottomDockPanel' + cDefaultNameSuffix);
    FBottomSplitter := CreateSplitter(alBottom, 'BottomSplitter' + cDefaultNameSuffix);
  finally
    ControlList.Free;
  end;
end;

procedure TJvDockServer.CreateSplitterStyle;
begin
  FLeftSplitterStyle := TJvDockSplitterStyle.Create(FLeftSplitter, crHSplit);
  FTopSplitterStyle := TJvDockSplitterStyle.Create(FTopSplitter, crVSplit);
  FRightSplitterStyle := TJvDockSplitterStyle.Create(FRightSplitter, crHSplit);
  FBottomSplitterStyle := TJvDockSplitterStyle.Create(FBottomSplitter, crVSplit);

  FLeftSplitterStyle.FDockServer := Self;
  FTopSplitterStyle.FDockServer := Self;
  FRightSplitterStyle.FDockServer := Self;
  FBottomSplitterStyle.FDockServer := Self;
end;

procedure TJvDockServer.DestroySplitterStyle;
begin
  FLeftSplitterStyle.Free;
  FTopSplitterStyle.Free;
  FRightSplitterStyle.Free;
  FBottomSplitterStyle.Free;
end;

procedure TJvDockServer.SetLeftSplitterStyle(const Value: TJvDockSplitterStyle);
begin
  FLeftSplitterStyle.Assign(Value);
end;

procedure TJvDockServer.SetTopSplitterStyle(const Value: TJvDockSplitterStyle);
begin
  FTopSplitterStyle.Assign(Value);
end;

procedure TJvDockServer.SetRightSplitterStyle(const Value: TJvDockSplitterStyle);
begin
  FRightSplitterStyle.Assign(Value);
end;

procedure TJvDockServer.SetBottomSplitterStyle(const Value: TJvDockSplitterStyle);
begin
  FBottomSplitterStyle.Assign(Value);
end;

procedure TJvDockServer.SetSplitterStyle;
begin
  LeftSplitterStyle.Splitter := FLeftSplitter;
  LeftSplitterStyle.SetSplitterStyle;
  TopSplitterStyle.Splitter := FTopSplitter;
  TopSplitterStyle.SetSplitterStyle;
  RightSplitterStyle.Splitter := FRightSplitter;
  RightSplitterStyle.SetSplitterStyle;
  BottomSplitterStyle.Splitter := FBottomSplitter;
  BottomSplitterStyle.SetSplitterStyle;
end;

procedure TJvDockServer.WindowProc(var Msg: TMessage);
begin
  if Assigned(FDockStyle) then
    if FDockStyle.DockServerWindowProc(Self, Msg) then
      Exit;
  if not (csDesigning in ComponentState) then
    if Msg.Msg = WM_ACTIVATE then
      WMActivate(TWMActivate(Msg));
  inherited WindowProc(Msg);
end;

function TJvDockServer.GetDockPanel(Index: Integer): TJvDockPanel;
begin
  Result := nil;
  case Index of
    0:
      Result := FTopDockPanel;
    1:
      Result := FBottomDockPanel;
    2:
      Result := FLeftDockPanel;
    3:
      Result := FRightDockPanel;
  end;
end;

function TJvDockServer.GetDockSplitter(Index: Integer): TJvDockSplitter;
begin
  Result := nil;
  case Index of
    0:
      Result := FTopSplitter;
    1:
      Result := FBottomSplitter;
    2:
      Result := FLeftSplitter;
    3:
      Result := FRightSplitter;
  end;
end;

procedure TJvDockServer.SetBottomDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(BottomDockPanel);
  inherited SetBottomDock(Value);
end;

procedure TJvDockServer.SetEnableDock(const Value: Boolean);
begin
  if not Value then
  begin
    DoFloatDockClients(TopDockPanel);
    DoFloatDockClients(BottomDockPanel);
    DoFloatDockClients(LeftDockPanel);
    DoFloatDockClients(RightDockPanel);
  end;
  inherited SetEnableDock(Value);
end;

procedure TJvDockServer.SetLeftDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(LeftDockPanel);
  inherited SetLeftDock(Value);
end;

procedure TJvDockServer.SetRightDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(RightDockPanel);
  inherited SetRightDock(Value);
end;

procedure TJvDockServer.SetTopDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(TopDockPanel);
  inherited SetTopDock(Value);
end;

procedure TJvDockServer.DoFloatDockClients(DockPanel: TJvDockPanel);
var
  I: Integer;
  ADockClient: TJvDockClient;
begin
  if not (csDesigning in ComponentState) and (DockPanel <> nil) then
    for I := DockPanel.DockClientCount - 1 downto 0 do
    begin
      ADockClient := FindDockClient(DockPanel.DockClients[I]);
      if ADockClient <> nil then
        ADockClient.RestoreChild;
    end;
end;

procedure TJvDockServer.WMActivate(var Msg: TWMActivate);
var
  I: TAlign;
  Control: TWinControl;
begin
  if Msg.Active = WA_INACTIVE then
    for I := alTop to alRight do
      DockPanelWithAlign[I].JvDockManager.ActiveControl := nil
  else
  if AutoFocusDockedForm then
  begin
    Control := GetActiveControl(ParentForm);
    for I := alTop to alRight do
      if GetHostDockParent(Control) = DockPanelWithAlign[I] then
      begin
        DockPanelWithAlign[I].JvDockManager.ActiveControl := Control;
        if Control.CanFocus then
          Control.SetFocus;
      end;
  end;
end;

procedure TJvDockServer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

procedure TJvDockServer.DoGetClientAlignControl(Align: TAlign; var Value: Integer);
begin
  if Assigned(FOnGetClientAlignSize) then
    FOnGetClientAlignSize(Align, Value);
end;

procedure TJvDockServer.DoFinishSetDockPanelSize(DockPanel: TJvDockPanel);
begin
  if Assigned(FOnFinishSetDockPanelSize) then
    FOnFinishSetDockPanelSize(DockPanel);
end;

function TJvDockServer.GetClientAlignControl(Align: TAlign): Integer;
begin
  Result := GetClientAlignControlArea(ParentForm, Align);
  DoGetClientAlignControl(Align, Result);
end;

function TJvDockServer.GetDockPanelWithAlign(Index: TAlign): TJvDockPanel;
begin
  Result := nil;
  case Index of
    alLeft:
      Result := FLeftDockPanel;
    alRight:
      Result := FRightDockPanel;
    alTop:
      Result := FTopDockPanel;
    alBottom:
      Result := FBottomDockPanel;
  end;
end;

function TJvDockServer.GetDockSplitterWithAlign(Index: TAlign): TJvDockSplitter;
begin
  Result := nil;
  case Index of
    alLeft:
      Result := FLeftSplitter;
    alRight:
      Result := FRightSplitter;
    alTop:
      Result := FTopSplitter;
    alBottom:
      Result := FBottomSplitter;
  end;
end;

//=== { TJvDockClient } ======================================================

constructor TJvDockClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParentVisible := ParentForm.Visible;
  ParentForm.DragKind := dkDock;
  ParentForm.DragMode := dmAutomatic;
  ParentForm.UseDockManager := False;
  if not (ParentForm is TJvDockableForm) then
    SetDockSite(ParentForm, True);
  LRDockWidth := 100;
  TBDockHeight := 100;
  if JvGlobalDockClient = nil then
    JvGlobalDockClient := Self;
  FDirectDrag := False;
  FShowHint := True;
  FCanFloat := True;
  FRelativeServer := nil;
  FDockLevel := 0;
  EnableCloseButton := True;
end;

destructor TJvDockClient.Destroy;
begin
  if not (ParentForm is TJvDockableForm) then
    SetDockSite(ParentForm, False);
  ParentForm.DragKind := dkDrag;
  ParentForm.DragMode := dmManual;
  inherited Destroy;
end;

procedure TJvDockClient.Loaded;
begin
  if Assigned(DockStyle) and Assigned(DockStyle.ConjoinPanelClass) then
    FConjoinPanelClass := DockStyle.ConjoinPanelClass
  else
    FConjoinPanelClass := DefaultConjoinPanelClass;

  if Assigned(DockStyle) and Assigned(DockStyle.TabDockClass) then
    FTabDockClass := DockStyle.TabDockClass
  else
    FTabDockClass := DefaultTabDockClass;
  inherited Loaded;
end;

procedure TJvDockClient.Assign(Source: TPersistent);
begin
  if Source is TJvDockClient then
  begin
    FConjoinPanelClass := TJvDockClient(Source).FConjoinPanelClass;
    FTabDockClass := TJvDockClient(Source).FTabDockClass;
    FParentVisible := TJvDockClient(Source).FParentVisible;
    FNCPopupMenu := TJvDockClient(Source).FNCPopupMenu;
    FDirectDrag := TJvDockClient(Source).FDirectDrag;
    FShowHint := TJvDockClient(Source).FShowHint;
    FCanFloat := TJvDockClient(Source).FCanFloat;
    FRelativeServer := TJvDockClient(Source).FRelativeServer;
    FDockLevel := TJvDockClient(Source).DockLevel;
  end
  else
    inherited Assign(Source);
end;

procedure TJvDockClient.WindowProc(var Msg: TMessage);
var
  OldOrient: TDockOrientation;
begin
  if Assigned(FDockStyle) then
    if FDockStyle.DockClientWindowProc(Self, Msg) then
      Exit;
  if not (csDesigning in ComponentState) then
  begin
    case Msg.Msg of
      CM_SHOWINGCHANGED:
        if IsWinXP and JvGlobalDockIsLoading then
          Exit;
      WM_NCLBUTTONDOWN:
        begin
          WMNCLButtonDown(TWMNCHitMessage(Msg));
          if Msg.Result = 1 then
            Exit;
        end;
      WM_NCLBUTTONUP:
        WMNCLButtonUp(TWMNCHitMessage(Msg));
      WM_NCLBUTTONDBLCLK:
        WMNCLButtonDblClk(TWMNCHitMessage(Msg));
      WM_NCMBUTTONDOWN:
        WMNCMButtonDown(TWMNCHitMessage(Msg));
      WM_NCMBUTTONUP:
        WMNCMButtonUp(TWMNCHitMessage(Msg));
      WM_NCMBUTTONDBLCLK:
        WMNCMButtonDblClk(TWMNCHitMessage(Msg));
      WM_NCRBUTTONDOWN:
        begin
          WMNCRButtonDown(TWMNCHitMessage(Msg));
          if FNCPopupMenu <> nil then
            Exit;
        end;
      WM_NCRBUTTONUP:
        WMNCRButtonUp(TWMNCHitMessage(Msg));
      WM_NCRBUTTONDBLCLK:
        WMNCRButtonDblClk(TWMNCHitMessage(Msg));
      WM_NCMOUSEMOVE:
        WMNCMOUSEMOVE(TWMNCHitMessage(Msg));
      WM_SIZE:
        WMSize(TWMSize(Msg));
      WM_ACTIVATE:
        WMActivate(TWMActivate(Msg));
      WM_WINDOWPOSCHANGED:
        begin
          ParentForm.ControlState := ParentForm.ControlState + [csDocking];
          OldOrient := ParentForm.DockOrientation;
          ParentForm.DockOrientation := doNoOrient;
          try
            inherited WindowProc(Msg);
          finally
            ParentForm.ControlState := ParentForm.ControlState - [csDocking];
            ParentForm.DockOrientation := OldOrient;
          end;
          Exit;
        end;
      CM_ENTER:
        Activate;
      CM_EXIT:
        Deactivate;
      CM_VISIBLECHANGED:
        CMVisibleChanged(Msg);
    end;
  end;

  inherited WindowProc(Msg);

  if Msg.Msg = WM_SETTEXT then
    if ParentForm.HostDockSite is TJvDockCustomControl then
      TJvDockCustomControl(ParentForm.HostDockSite).UpdateCaption(ParentForm);
end;

function TJvDockClient.CreateConjoinHostAndDockControl(Control1, Control2: TControl;
  DockType: TAlign): TJvDockConjoinHostForm;
var
  APanel: TJvDockConjoinPanel;
  OldDockWidth, OldDockHeight: Integer;
begin
  Result := TJvDockConjoinHostForm.Create(Application);

  APanel := CreateConjoinPanelClass(Result);

  Result.BoundsRect := Control1.BoundsRect;

  Result.Width := Control1.UndockWidth;
  Result.Height := Control1.UndockHeight;

  OldDockWidth := Control1.LRDockWidth;
  OldDockHeight := Control1.TBDockHeight;
  Control1.ManualDock(APanel, nil, alNone);
  Control1.LRDockWidth := OldDockWidth;
  Control1.TBDockHeight := OldDockHeight;

  OldDockWidth := Control2.LRDockWidth;
  OldDockHeight := Control2.TBDockHeight;
  Control2.ManualDock(APanel, nil, DockType);
  Control2.LRDockWidth := OldDockWidth;
  Control2.TBDockHeight := OldDockHeight;

  SetDockSite(Result, False);
end;

function TJvDockClient.CreateTabHostAndDockControl(Control1, Control2: TControl): TJvDockTabHostForm;
var
  Page: TJvDockTabPageControl;
  OldDockWidth, OldDockHeight: Integer;
begin
  Result := TJvDockTabHostForm.Create(Application);

  Page := CreateTabDockClass(Result);

  Result.BoundsRect := Control1.BoundsRect;

  Result.Width := Control1.UndockWidth;
  Result.Height := Control1.UndockHeight;

  OldDockWidth := Control1.LRDockWidth;
  OldDockHeight := Control1.TBDockHeight;
  Control1.ManualDock(Page, nil, alClient);
  Control1.LRDockWidth := OldDockWidth;
  Control1.TBDockHeight := OldDockHeight;

  OldDockWidth := Control2.LRDockWidth;
  OldDockHeight := Control2.TBDockHeight;
  Control2.ManualDock(Page, nil, alClient);
  Control2.LRDockWidth := OldDockWidth;
  Control2.TBDockHeight := OldDockHeight;

  SetDockSite(Result, False);
end;

procedure TJvDockClient.MakeHideEvent;
begin
  ParentVisible := False;
  if Assigned(FOnFormHide) then
    FOnFormHide(Self);
end;

procedure TJvDockClient.MakeShowEvent;
begin
  if ParentForm.Visible then
  begin
    if Assigned(FOnFormShow) then
      FOnFormShow(Self);
    ParentVisible := True;
  end;
end;

procedure TJvDockClient.SetParentVisible(const Value: Boolean);
begin
  FParentVisible := Value;
end;

procedure TJvDockClient.DoFloatDockClients(PanelAlign: TAlign);
begin
  if (ParentForm.HostDockSite is TJvDockPanel) and
    (PanelAlign = ParentForm.HostDockSite.Align) then
    RestoreChild;
end;

procedure TJvDockClient.SetBottomDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alBottom);
  inherited SetBottomDock(Value);
end;

procedure TJvDockClient.SetEachOtherDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockEachOther;
  inherited SetEachOtherDock(Value);
end;

procedure TJvDockClient.SetEnableDock(const Value: Boolean);
begin
  if not Value then
  begin
    DoFloatDockClients(alTop);
    DoFloatDockClients(alBottom);
    DoFloatDockClients(alLeft);
    DoFloatDockClients(alRight);
    DoFloatDockEachOther;
  end;
  if ParentForm <> nil then
    if Value then
      ParentForm.DragKind := dkDock
    else
      ParentForm.DragKind := dkDrag;
  inherited SetEnableDock(Value);
end;

procedure TJvDockClient.SetLeftDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alLeft);
  inherited SetLeftDock(Value);
end;

procedure TJvDockClient.SetRightDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alRight);
  inherited SetRightDock(Value);
end;

procedure TJvDockClient.SetTopDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alTop);
  inherited SetTopDock(Value);
end;

procedure TJvDockClient.DoFloatDockEachOther;
begin
  if (ParentForm.HostDockSite <> nil) and
    (ParentForm.HostDockSite.Parent is TJvDockableForm) then
    RestoreChild;
end;

procedure TJvDockClient.WMSize(var Msg: TWMSize);
begin
  inherited;
end;

function TJvDockClient.CreateConjoinPanelClass(ConjoinHost: TForm): TJvDockConjoinPanel;
begin
  Result := nil;
  TJvDockConjoinHostForm(ConjoinHost).DockClient.Assign(Self);
  if (FConjoinPanelClass <> nil) and
    (FConjoinPanelClass <> TJvDockConjoinPanelClass(ClassType)) then
  begin
    Result := FConjoinPanelClass.Create(ConjoinHost);
    Result.Align := alClient;
    TJvDockConjoinHostForm(ConjoinHost).DockableControl := Result;
    TJvDockConjoinHostForm(ConjoinHost).Panel := Result;
    SetDockSite(Result, True);

    DockStyle.AssignConjoinServerOption(TJvDockConjoinHostForm(ConjoinHost).Panel);
  end;
end;

function TJvDockClient.CreateTabDockClass(TabHost: TForm): TJvDockTabPageControl;
begin
  Result := nil;
  TJvDockTabHostForm(TabHost).DockClient.Assign(Self);
  if (FTabDockClass <> nil) and
    (FTabDockClass <> TJvDockTabClass(ClassType)) then
  begin
    Result := FTabDockClass.Create(TabHost);
    Result.Align := alClient;
    TJvDockTabHostForm(TabHost).DockableControl := Result;
    TJvDockTabHostForm(TabHost).PageControl := Result;
    SetDockSite(Result, True);

    DockStyle.AssignTabServerOption(TJvDockTabHostForm(TabHost).PageControl);
  end;
end;

procedure TJvDockClient.WMActivate(var Msg: TWMActivate);
begin
  if ParentForm is TJvDockConjoinHostForm then
    if Msg.Active = WA_INACTIVE then
      TJvDockConjoinPanel(TJvDockConjoinHostForm(ParentForm).Panel).JvDockManager.ActiveControl := nil
    else
      TJvDockConjoinPanel(TJvDockConjoinHostForm(ParentForm).Panel).JvDockManager.ActiveControl :=
        GetActiveControl(ParentForm);
end;

procedure TJvDockClient.Activate;
begin
  if ParentForm.HostDockSite is TJvDockCustomPanel then
    TJvDockCustomPanel(ParentForm.HostDockSite).JvDockManager.ActiveControl := ParentForm;
end;

procedure TJvDockClient.Deactivate;
begin
  if ParentForm.HostDockSite is TJvDockCustomPanel then
    if TJvDockCustomPanel(ParentForm.HostDockSite).JvDockManager <> nil then
      TJvDockCustomPanel(ParentForm.HostDockSite).JvDockManager.ActiveControl := nil;
end;

procedure TJvDockClient.DoFormOnClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Action = caHide then
  begin
    HideDockForm(ParentForm);
    FParentVisible := True;
  end;
end;

procedure TJvDockClient.FormDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  if Assigned(DockStyle) then
    DockStyle.FormDockDrop(Self, Source, X, Y);
end;

procedure TJvDockClient.FormDockOver(Source: TJvDockDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  FormPositionDockRect(Source);
  if Assigned(DockStyle) then
    DockStyle.FormDockOver(Self, Source, X, Y, State, Accept);
end;

procedure TJvDockClient.FormEndDock(Target: TObject; X, Y: Integer);
begin
  if Assigned(DockStyle) then
    DockStyle.FormEndDock(Self, Target, X, Y);
end;

procedure TJvDockClient.FormPositionDockRect(Source: TJvDockDragDockObject);
begin
  if Assigned(DockStyle) then
    DockStyle.FormPositionDockRect(Self, Source);
end;

procedure TJvDockClient.FormStartDock(var Source: TJvDockDragDockObject);
begin
  if Assigned(DockStyle) then
    DockStyle.FormStartDock(Self, Source);
end;

function TJvDockClient.FormUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  if Assigned(DockStyle) then
    Result := DockStyle.FormUnDock(Self, Newtarget, Client)
  else
    Result := False;
end;

procedure TJvDockClient.FormGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  GetWindowRect(ParentForm.Handle, InfluenceRect);
  InflateRect(InfluenceRect, -4, -4);
  if Assigned(DockStyle) then
    DockStyle.FormGetSiteInfo(Source, Self, Client, InfluenceRect, MousePos, CanDock);
end;

function TJvDockClient.GetLRDockWidth: Integer;
begin
  Result := ParentForm.LRDockWidth;
end;

function TJvDockClient.GetTBDockHeight: Integer;
begin
  Result := ParentForm.TBDockHeight;
end;

procedure TJvDockClient.SetLRDockWidth(const Value: Integer);
begin
  if ParentForm.LRDockWidth <> Value then
    ParentForm.LRDockWidth := Value;
end;

procedure TJvDockClient.SetTBDockHeight(const Value: Integer);
begin
  if ParentForm.TBDockHeight <> Value then
    ParentForm.TBDockHeight := Value;
end;

procedure TJvDockClient.DoNCButtonDown(Msg: TWMNCHitMessage;
  Button: TMouseButton; MouseStation: TJvDockMouseStation);
begin
  if Assigned(FOnNCButtonDown) then
    FOnNCButtonDown(Self, Button, Msg.XCursor, Msg.YCursor,
      Msg.HitTest, MouseStation);
end;

procedure TJvDockClient.DoNCButtonUp(Msg: TWMNCHitMessage;
  Button: TMouseButton; MouseStation: TJvDockMouseStation);
begin
  if Assigned(FOnNCButtonUp) then
    FOnNCButtonUp(Self, Button, Msg.XCursor, Msg.YCursor,
      Msg.HitTest, MouseStation);
  if Button = mbRight then
    DoMenuPopup(Msg.XCursor, Msg.YCursor);
end;

procedure TJvDockClient.DoNCMouseMove(Msg: TWMNCHitMessage;
  MouseStation: TJvDockMouseStation);
begin
  if Assigned(FOnNCMouseMove) then
    FOnNCMouseMove(Self, Msg.XCursor, Msg.YCursor,
      Msg.HitTest, MouseStation);
end;

procedure TJvDockClient.DoNCButtonDblClk(Msg: TWMNCHitMessage; Button: TMouseButton;
  MouseStation: TJvDockMouseStation);
begin
  if Assigned(FOnNCButtonDblClk) then
    FOnNCButtonDblClk(Self, Button, Msg.XCursor, Msg.YCursor,
      Msg.HitTest, MouseStation);
end;

procedure TJvDockClient.WMNCLButtonDown(var Msg: TWMNCHitMessage);
begin
  DoNCButtonDown(Msg, mbLeft, msFloat);

  JvGlobalDockClient := Self;

  if (Msg.HitTest = HTCAPTION) and (ParentForm.DragKind = dkDock) and not
    (csDesigning in ComponentState) and not IsIconic(ParentForm.Handle) then
  begin
    SetWindowPos(ParentForm.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE);
    PostMessage(ParentForm.Handle, WM_NCLBUTTONUP, TMessage(Msg).WParam, TMessage(Msg).LParam);
    if ParentForm.Active then
      JvGlobalDockManager.BeginDrag(ParentForm, DirectDrag, Integer(DirectDrag) * 2 - 1);
    Msg.Result := 1;
  end
  else
    Msg.Result := 0;
end;

procedure TJvDockClient.WMNCLButtonUp(var Msg: TWMNCHitMessage);
begin
  DoNCButtonUp(Msg, mbLeft, msFloat);
end;

procedure TJvDockClient.WMNCMButtonDown(var Msg: TWMNCHitMessage);
begin
  DoNCButtonDown(Msg, mbMiddle, msFloat);
end;

procedure TJvDockClient.WMNCMButtonUp(var Msg: TWMNCHitMessage);
begin
  DoNCButtonUp(Msg, mbMiddle, msFloat);
end;

procedure TJvDockClient.WMNCRButtonDown(var Msg: TWMNCHitMessage);
begin
  DoNCButtonDown(Msg, mbRight, msFloat);
end;

procedure TJvDockClient.WMNCRButtonUp(var Msg: TWMNCHitMessage);
begin
  DoNCButtonUp(Msg, mbRight, msFloat);
end;

procedure TJvDockClient.WMNCMOUSEMOVE(var Msg: TWMNCHitMessage);
begin
  DoNCMouseMove(Msg, msFloat);
end;

procedure TJvDockClient.WMNCLButtonDblClk(var Msg: TWMNCHitMessage);
begin
  DoNCButtonDblClk(Msg, mbLeft, msFloat);
end;

procedure TJvDockClient.WMNCMButtonDblClk(var Msg: TWMNCHitMessage);
begin
  DoNCButtonDblClk(Msg, mbMiddle, msFloat);
end;

procedure TJvDockClient.WMNCRButtonDblClk(var Msg: TWMNCHitMessage);
begin
  DoNCButtonDblClk(Msg, mbRight, msFloat);
end;

procedure TJvDockClient.SetNCPopupMenu(const Value: TPopupMenu);
begin
  FNCPopupMenu := Value;
end;

procedure TJvDockClient.DoMenuPopup(X, Y: Integer);
begin
  if FNCPopupMenu <> nil then
  begin
    FNCPopupMenu.PopupComponent := ParentForm;
    FNCPopupMenu.Popup(X, Y);
  end;
end;

procedure TJvDockClient.DoPaintDockGrabber(Canvas: TCanvas;
  Control: TControl; const ARect: TRect);
begin
  if Assigned(FOnPaintDockGrabber) then
    FOnPaintDockGrabber(Canvas, Control, ARect);
end;

procedure TJvDockClient.DoPaintDockSplitter(Canvas: TCanvas;
  Control: TControl; const ARect: TRect);
begin
  if Assigned(FOnPaintDockSplitter) then
    FOnPaintDockSplitter(Canvas, Control, ARect);
end;

procedure TJvDockClient.FormGetDockEdge(Source: TJvDockDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
begin
  if Assigned(DockStyle) then
    DockStyle.FormGetDockEdge(Self, Source, MousePos, DropAlign)
  else
    DropAlign := alNone;
end;

procedure TJvDockClient.DoFormShowHint(HTFlag: Integer; var HintStr: string;
  var CanShow: Boolean);
begin
  if Assigned(FOnFormShowHint) then
    FOnFormShowHint(HTFlag, HintStr, CanShow);
end;

procedure TJvDockClient.SetCurrentDockSite(const Value: TWinControl);
begin
  FCurrentDockSite := Value;
end;

procedure TJvDockClient.SetLastDockSite(const Value: TWinControl);
begin
  FLastDockSite := Value;
end;

procedure TJvDockClient.SetVSPaneWidth(const Value: Integer);
begin
  FVSPaneWidth := Value;
end;

procedure TJvDockClient.RestoreChild;
begin
  DockStyle.RestoreClient(Self);
end;

procedure TJvDockClient.SetUnDockLeft(const Value: Integer);
begin
  FUnDockLeft := Value;
end;

procedure TJvDockClient.SetUnDockTop(const Value: Integer);
begin
  FUnDockTop := Value;
end;

procedure TJvDockClient.HideParentForm;
begin
  HideDockForm(ParentForm);
end;

procedure TJvDockClient.ShowParentForm;
begin
  ShowDockForm(ParentForm);
end;

function TJvDockClient.GetDockState: Integer;
begin
  Result := JvDockState_Unknown;
  if DockStyle <> nil then
    Result := DockStyle.GetDockState(Self);
end;

procedure TJvDockClient.CMVisibleChanged(var Msg: TMessage);
begin
end;

procedure TJvDockClient.SetCanFloat(const Value: Boolean);
begin
  FCanFloat := Value;
end;

procedure TJvDockClient.SetRelativeServer(const Value: TJvDockServer);
begin
  if csDesigning in ComponentState then
    if Value <> nil then
      Value.FreeNotification(ParentForm);
  FRelativeServer := Value;
end;

procedure TJvDockClient.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FRelativeServer) then
    FRelativeServer := nil;
end;

procedure TJvDockClient.SetDockLevel(const Value: Integer);
begin
  if not ParentForm.Floating then
    if FDockLevel <> Value then
      DoFloatForm(ParentForm);
  FDockLevel := Value;
end;

procedure TJvDockClient.SetEnableCloseButton(const Value: Boolean);
begin
  FEnableCloseButton := Value;
end;

//=== { TJvDockableForm } ====================================================

constructor TJvDockableForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DragKind := dkDock;
  FDockClient := TJvDockClient.Create(Self);
  JvGlobalDockManager.DockableFormList.Add(Self);
  FFloatingChild := nil;
  TBDockHeight := FDockClient.TBDockHeight;
  LRDockWidth := FDockClient.LRDockWidth;
end;

destructor TJvDockableForm.Destroy;
var
  Index: Integer;
begin
  if JvGlobalDockManager <> nil then
  begin
    Index := JvGlobalDockManager.DockableFormList.IndexOf(Self);
    if Index <> -1 then
      JvGlobalDockManager.DockableFormList.Delete(Index);
  end;
  if DockClient.LastDockSite is TJvDockPanel then
    TJvDockPanel(DockClient.LastDockSite).JvDockManager.RemoveControl(Self);
  inherited Destroy;
  // (rom) better comment this
  FFloatingChild := nil;
end;

procedure TJvDockableForm.DoClose(var Action: TCloseAction);
var
  I: Integer;
begin
  if DockableControl.DockClientCount = 1 then
  begin
    FFloatingChild := DockableControl.DockClients[0];

    if HostDockSite <> nil then
      FFloatingChild.Visible := False;

    DoFloat(Self, DockableControl.DockClients[0]);
    Action := caFree;
  end
  else
  if DockableControl.DockClientCount = 0 then
    Action := caFree
  else
  begin
    Action := caHide;
    if (FUnDockControl <> nil) and (DockableControl.DockClientCount = 2) then
      for I := 0 to DockableControl.DockClientCount - 1 do
        if FUnDockControl = DockableControl.DockClients[I] then
        begin
          Action := caNone;
          Break;
        end;
  end;
  if (HostDockSite is TJvDockPanel) and (HostDockSite.VisibleDockClientCount = 1) and
    (FFloatingChild = nil) then
    TJvDockPanel(HostDockSite).ShowDockPanel(False, Self);

  inherited DoClose(Action);
  FUnDockControl := nil;
end;

function TJvDockableForm.GetDockableControl: TWinControl;
begin
  Result := FDockableControl;
end;

procedure TJvDockableForm.SetDockableControl(const Value: TWinControl);
begin
  FDockableControl := Value;
end;

procedure TJvDockableForm.SetUnDockControl(const Value: TControl);
begin
  FUnDockControl := Value;
end;

//=== { TJvDockConjoinHostForm } =============================================

constructor TJvDockConjoinHostForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := ConjoinDockHostBorderStyle;
end;

procedure TJvDockConjoinHostForm.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
end;

//=== { TJvDockTabHostForm } =================================================

constructor TJvDockTabHostForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := TabDockHostBorderStyle;
end;

function TJvDockTabHostForm.GetActiveDockForm: TForm;
begin
  if PageControl.ActivePage.ControlCount = 1 then
    Result := TForm(PageControl.ActivePage.Controls[0])
  else
    Result := nil;
end;

//=== { TJvDockConjoinPanel } ================================================

constructor TJvDockConjoinPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  DockClient := TJvDockConjoinHostForm(AOwner).DockClient;
  Align := alClient;
  BevelOuter := bvNone;
  DoubleBuffered := True;
  ParentFont := False;
  Caption := '';
end;

procedure TJvDockConjoinPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  if Perform(CM_DOCKCLIENT, Integer(Source), Integer(SmallPoint(X, Y))) >= 0 then
  begin
    if Source.Control is TForm then
    begin
      ParentForm.ActiveControl := nil;
      TForm(Source.Control).ActiveControl := nil;

      SetDockSite(TForm(Source.Control), False);
      if TForm(Source.Control).FormStyle = fsStayOnTop then
        TForm(Parent).FormStyle := fsStayOnTop;
    end;
    UpdateCaption(nil);
  end;
end;

procedure TJvDockConjoinPanel.DoDockOver(Source: TDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
end;

function TJvDockConjoinPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  ParentForm.FUnDockControl := Client;

  if not (Client is TJvDockableForm) then
    SetDockSite(TForm(Client), True);
  if (VisibleDockClientCount = 1) or
    (DockClientCount <= 2) then
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
  Result := Perform(CM_UNDOCKCLIENT, Integer(NewTarget), Integer(Client)) = 0;
end;

function TJvDockConjoinPanel.GetParentForm: TJvDockConjoinHostForm;
begin
  Result := TJvDockConjoinHostForm(Parent);
end;

procedure TJvDockConjoinPanel.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := IsDockable(Self, Client);
  if CanDock then
    GetWindowRect(Handle, InfluenceRect);
end;

procedure TJvDockConjoinPanel.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  inherited CustomDockDrop(Source, X, Y);
  if Source.Control is TForm then
  begin
    ParentForm.ActiveControl := nil;
    if TForm(Source.Control).FormStyle = fsStayOnTop then
      TForm(Parent).FormStyle := fsStayOnTop;
  end;
  UpdateCaption(nil);
end;

procedure TJvDockConjoinPanel.CustomDockOver(Source: TJvDockDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
end;

procedure TJvDockConjoinPanel.CustomEndDock(Target: TObject; X, Y: Integer);
begin
  inherited CustomEndDock(Target, X, Y);
end;

procedure TJvDockConjoinPanel.CustomGetSiteInfo(Source: TJvDockDragDockObject;
  Client: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
  inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock);
end;

procedure TJvDockConjoinPanel.CustomPositionDockRect(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  inherited CustomPositionDockRect(Source, X, Y);
end;

procedure TJvDockConjoinPanel.CustomStartDock(var Source: TJvDockDragDockObject);
begin
  ParentForm.FUnDockControl := nil;
  inherited CustomStartDock(Source);
end;

function TJvDockConjoinPanel.CustomUnDock(Source: TJvDockDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  ParentForm.FUnDockControl := Client;

  if not (Client is TJvDockableForm) then
    SetDockSite(TForm(Client), True);
  if ((VisibleDockClientCount = 1) or
    (DockClientCount <= 2)) and (NewTarget <> ParentForm.DockableControl) then
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
  Result := inherited CustomUnDock(Source, NewTarget, Client);
end;

procedure TJvDockConjoinPanel.ReloadDockedControl(const AControlName: string;
  var AControl: TControl);
begin
  AControl := JvDockFindDockFormWithName(AControlName);
end;

procedure TJvDockConjoinPanel.CMUnDockClient(var Msg: TCMUnDockClient);
begin
  inherited;
  if (DockClientCount = 2) and (VisibleDockClientCount = 1) then
    PostMessage(ParentForm.Handle, WM_CLOSE, 0, 0);
  if VisibleDockClientCount <= 2 then
    JvDockControlForm.UpdateCaption(Self, Msg.Client);
  if UseDockManager and (JvDockManager <> nil) then
    JvDockManager.ResetBounds(True);
end;

//=== { TJvDockTabPageControl } ==============================================

constructor TJvDockTabPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  DockClient := FindDockClient(Parent);
  SetDockSite(Self, True);
  PopupMenu := DockPageControlPopupMenu;
  HotTrack := DockPageControlHotTrack;
  DoubleBuffered := True;
  Caption := '';
  FVersion := $00040000;
end;

destructor TJvDockTabPageControl.Destroy;
begin
  SetDockSite(Self, False);
  inherited Destroy;
end;

procedure TJvDockTabPageControl.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  case TabPosition of
    tpLeft:
      Inc(Rect.Left, 2);
    tpRight:
      Dec(Rect.Right, 2);
    tpBottom:
      begin
        Dec(Rect.Top, 1);
        Dec(Rect.Bottom, 2);
      end;
  end;
end;

procedure TJvDockTabPageControl.DockDrop(Source: TDragDockObject; X,
  Y: Integer);
begin
  if Perform(CM_DOCKCLIENT, Integer(Source), Integer(SmallPoint(X, Y))) >= 0 then
  begin
    if Source.Control is TForm then
    begin
      TForm(Source.Control).ActiveControl := nil;
      SetDockSite(TWinControl(Source.Control), False);
      if TForm(Source.Control).FormStyle = fsStayOnTop then
        TForm(Parent).FormStyle := fsStayOnTop;
    end;
    UpdateCaption(nil);
  end;
end;

function TJvDockTabPageControl.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  if not (Client is TJvDockableForm) then
    SetDockSite(TForm(Client), True);
  if (VisibleDockClientCount = 1) or
    (DockClientCount <= 2) then
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
  Result := Perform(CM_UNDOCKCLIENT, Integer(NewTarget), Integer(Client)) = 0;
end;

function TJvDockTabPageControl.GetParentForm: TJvDockTabHostForm;
begin
  Result := TJvDockTabHostForm(Parent);
end;

procedure TJvDockTabPageControl.LoadFromStream(Stream: TStream);
var
  I, ACount, NameLen, SheetVisible, ActiveSheetIndex: Integer;
  ControlName: string;
  AControl: TControl;
begin
  Stream.Read(I, SizeOf(I));

  Stream.Read(ACount, SizeOf(ACount));
  for I := 0 to ACount - 1 do
  begin
    ControlName := '';

    Stream.Read(NameLen, SizeOf(NameLen));
    if NameLen > 0 then
    begin
      SetLength(ControlName, NameLen);
      Stream.Read(Pointer(ControlName)^, NameLen);
    end;
    if ControlName <> '' then
    begin
      ReloadDockedControl(ControlName, AControl);
      if AControl <> nil then
        AControl.ManualDock(Self, nil, alClient);
    end;

    Stream.Read(SheetVisible, SizeOf(SheetVisible));
    DockClients[I].Visible := Boolean(SheetVisible);
  end;

  Stream.Read(ActiveSheetIndex, SizeOf(ActiveSheetIndex));
  ActivePageIndex := ActiveSheetIndex;
  Change;
end;

procedure TJvDockTabPageControl.SaveToStream(Stream: TStream);
var
  I, ACount, NameLen, SheetVisible, ActiveSheetIndex: Integer;
  ControlName: string;
  CurrentControl: TControl;
  TabPageStreamEndFlag: Integer;
begin
  Stream.Write(FVersion, SizeOf(FVersion));
  ACount := Count;

  Stream.Write(ACount, SizeOf(ACount));
  for I := 0 to ACount - 1 do
  begin
    if Pages[I].ControlCount > 0 then
    begin
      CurrentControl := Pages[I].Controls[0];
      ControlName := CurrentControl.Name;
      NameLen := Length(ControlName);

      Stream.Write(NameLen, SizeOf(NameLen));

      if NameLen > 0 then
        Stream.Write(Pointer(ControlName)^, NameLen);
      SheetVisible := 0;
      if (Self is TJvDockVSNETTabPageControl) and (ParentForm.HostDockSite is TJvDockPanel) then
        SheetVisible := Integer(TJvDockVSNETTabSheet(Pages[I]).OldVisible)
      else
        SheetVisible := SheetVisible + Integer(CurrentControl.Visible);

      Stream.Write(SheetVisible, SizeOf(SheetVisible));
    end;
  end;
  ActiveSheetIndex := ActivePageIndex;

  Stream.Write(ActiveSheetIndex, SizeOf(ActiveSheetIndex));

  TabPageStreamEndFlag := -10;
  Stream.Write(TabPageStreamEndFlag, SizeOf(TabPageStreamEndFlag));
end;

procedure TJvDockTabPageControl.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
var
  DragDockObject: TDragDockObject;
begin
  if Source.DropAlign in [alClient, alNone] then
  begin
    DragDockObject := TDragDockObject.Create(Source.Control);
    try
      DragDockObject.DockRect := Source.DockRect;
      DragDockObject.Control := Source.Control;
      Perform(CM_DOCKCLIENT, Integer(DragDockObject), Integer(SmallPoint(X, Y)));
      UpdateCaption(nil);
    finally
      DragDockObject.Free;
    end;
  end
  else
    inherited CustomDockDrop(Source, X, Y);
  if Source.Control is TForm then
  begin
    TForm(Source.Control).ActiveControl := nil;
    SetDockSite(TForm(Source.Control), False);
  end;
end;

procedure TJvDockTabPageControl.CustomDockOver(Source: TJvDockDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  Rect: TRect;
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);

  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);

  if Accept then
  begin
    ComputeDockingRect(Self, Rect, Point(ClientWidth div 2, ClientHeight div 2));
    Source.DockRect := Rect;
  end;
end;

procedure TJvDockTabPageControl.CustomEndDock(Target: TObject; X, Y: Integer);
begin
  inherited CustomEndDock(Target, X, Y);
end;

procedure TJvDockTabPageControl.CustomGetSiteInfo(Source: TJvDockDragDockObject;
  Client: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock);
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
end;

procedure TJvDockTabPageControl.CustomPositionDockRect(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  inherited CustomPositionDockRect(Source, X, Y);
end;

procedure TJvDockTabPageControl.CustomStartDock(var Source: TJvDockDragDockObject);
begin
  inherited CustomStartDock(Source);
end;

function TJvDockTabPageControl.CustomUnDock(Source: TJvDockDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  if not (Client is TJvDockableForm) then
    SetDockSite(TForm(Client), True);
  if (VisibleDockClientCount = 1) or (DockClientCount <= 2) then
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
  Result := Perform(CM_UNDOCKCLIENT, Integer(NewTarget), Integer(Client)) = 0;
end;

procedure TJvDockTabPageControl.ReloadDockedControl(const AControlName: string;
  var AControl: TControl);
begin
  AControl := JvDockFindDockFormWithName(AControlName);
end;

procedure TJvDockTabPageControl.CustomGetDockEdge(Source: TJvDockDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
var
  ARect: TRect;
begin
  ARect := Source.DockRect;
  DropAlign := ComputeDockingRect(Source.Control, ARect, MousePos);
end;

//=== { TJvDockBasicStyle } ==================================================

constructor TJvDockBasicStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  DockPanelClass := DefaultDockPanelClass;
  DockSplitterClass := DefaultDockSplitterClass;
  ConjoinPanelClass := DefaultConjoinPanelClass;
  TabDockClass := DefaultTabDockClass;
  DockPanelTreeClass := DefaultDockTreeClass;
  DockPanelZoneClass := DefaultDockZoneClass;
  ConjoinPanelTreeClass := DefaultDockTreeClass;
  ConjoinPanelZoneClass := DefaultDockZoneClass;
  FConjoinServerOptionClass := TJvDockBasicConjoinServerOption;
  FTabServerOptionClass := TJvDockBasicTabServerOption;
  FDockBaseControlList := TList.Create;
  if AOwner is TCustomForm then
    FParentForm := TForm(AOwner)
  else
    FParentForm := nil;

  if not (csDesigning in ComponentState) then
  begin
    FOldWindowProc := FParentForm.WindowProc;
    FParentForm.WindowProc := ParentFormWindowProc;
  end;
end;

destructor TJvDockBasicStyle.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FOldWindowProc) then
      FParentForm.WindowProc := FOldWindowProc;
    FOldWindowProc := nil;
  end;
  FDockBaseControlList.Free;
  FreeServerOption;
  inherited Destroy;
end;

{$IFNDEF USEJVCL}

function TJvDockBasicStyle.GetControlName: string;
begin
  Result := RsDockStyleName;
end;

function TJvDockBasicStyle.GetDockStyleVersion: string;
begin
  Result := RsDockStyleVersion;
end;

{$ENDIF USEJVCL}

procedure TJvDockBasicStyle.SetDockBaseControl(IsCreate: Boolean;
  DockBaseControl: TJvDockBaseControl);
begin
end;

procedure TJvDockBasicStyle.FormDockDrop(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject; X, Y: Integer);
var
  ARect, DRect: TRect;
  DockType: TAlign;
  Host: TCustomForm;
  APanelDock: TWinControl;
begin
  if IsDockable(DockClient.ParentForm, Source.Control, Source.DropOnControl, Source.DropAlign) then
  begin
    Host := nil;

    if not JvGlobalDockIsLoading then
      JvDockLockWindow(nil);
    try
      with DockClient do
      begin
        DockType := ComputeDockingRect(DockClient.ParentForm, ARect, Point(X, Y));

        if ParentForm.HostDockSite is TJvDockPanel then
        begin
          if DockType = alClient then
          begin
            if Source.Control is TJvDockTabHostForm then
            begin
              APanelDock := ParentForm.HostDockSite;
              ARect := ParentForm.BoundsRect;
              ParentForm.ManualDock(TJvDockTabHostForm(Source.Control).PageControl, nil, alClient);
              TJvDockTabHostForm(Source.Control).PageControl.ActivePage.PageIndex := 0;
              Source.Control.BoundsRect := ARect;
              Source.Control.ManualDock(APanelDock, nil, alClient);
              if ParentForm.FormStyle = fsStayOnTop then
                TForm(Source.Control).FormStyle := fsStayOnTop;
            end
            else
            begin
              APanelDock := ParentForm.HostDockSite;
              DRect.TopLeft := ParentForm.HostDockSite.ClientToScreen(Point(0, 0));
              Host := CreateTabHostAndDockControl(ParentForm, Source.Control);
              SetDockSite(ParentForm, False);
              SetDockSite(TWinControl(Source.Control), False);
              Host.Top := DRect.Top;
              Host.Left := DRect.Left;
              Host.ManualDock(APanelDock, nil, alClient);
              Host.Visible := True;
            end;
          end
          else
          begin
            DRect := ParentForm.HostDockSite.BoundsRect;
            Source.Control.ManualDock(ParentForm.HostDockSite, nil, DockType);
            ParentForm.HostDockSite.BoundsRect := DRect;
          end;
          Exit;
        end;

        if DockType = alClient then
        begin
          Host := CreateTabHostAndDockControl(ParentForm, Source.Control);
          SetDockSite(ParentForm, False);
          SetDockSite(TWinControl(Source.Control), False);
          Host.Visible := True;
        end
        else
        if DockType <> alNone then
        begin
          Host := CreateConjoinHostAndDockControl(ParentForm, Source.Control, DockType);
          SetDockSite(ParentForm, False);
          SetDockSite(TWinControl(Source.Control), False);
          Host.Visible := True;
        end;

        if Host <> nil then
        begin
          Host.LRDockWidth := Source.Control.LRDockWidth;
          Host.TBDockHeight := Source.Control.TBDockHeight;
        end;
      end;
    finally
      if not JvGlobalDockIsLoading then
        JvDockUnLockWindow;
    end;
  end;
end;

procedure TJvDockBasicStyle.FormDockOver(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
begin
  with DockClient do
  begin
    Accept := EnableDock and EachOtherDock and
      IsDockable(ParentForm.HostDockSite, Source.Control, Source.DropOnControl, Source.DropAlign);
    if Accept and (State = dsDragMove) and
      (ComputeDockingRect(ParentForm, ARect, Point(X, Y)) <> alNone) then
      Source.DockRect := ARect;
  end;
end;

procedure TJvDockBasicStyle.FormEndDock(DockClient: TJvDockClient;
  Target: TObject; X, Y: Integer);
begin
end;

procedure TJvDockBasicStyle.FormPositionDockRect(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject);
var
  NewWidth, NewHeight: Integer;
  TempX, TempY: Double;
  R: TRect;
begin
  with Source do
  begin
    if (DragTarget = nil) or (not TWinControlAccessProtected(DragTarget).UseDockManager) then
    begin
      NewWidth := Control.UndockWidth;
      NewHeight := Control.UndockHeight;

      TempX := DragPos.X - ((NewWidth) * MouseDeltaX);
      TempY := DragPos.Y - ((NewHeight) * MouseDeltaY);
      with DockRect do
      begin
        Left := Round(TempX);
        Top := Round(TempY);
        Right := Left + NewWidth;
        Bottom := Top + NewHeight;
      end;

      AdjustDockRect(DockRect);
    end
    else
    begin
      GetWindowRect(TargetControl.Handle, R);
      DockRect := R;
      if TWinControlAccessProtected(DragTarget).UseDockManager then
        if TargetControl is TJvDockCustomPanel then
          if (TJvDockCustomPanel(DragTarget).JvDockManager <> nil) then
          begin
            R := DockRect;
            TJvDockCustomPanel(DragTarget).JvDockManager.PositionDockRect(Control,
              DropOnControl, DropAlign, R);
            DockRect := R;
          end;
    end;
  end;
end;

procedure TJvDockBasicStyle.FormStartDock(DockClient: TJvDockClient;
  var Source: TJvDockDragDockObject);
begin
end;

function TJvDockBasicStyle.FormUnDock(DockClient: TJvDockClient; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  Result := True;
end;

procedure TJvDockBasicStyle.FormGetSiteInfo(Source: TJvDockDragDockObject;
  DockClient: TJvDockClient; Client: TControl; var InfluenceRect: TRect;
  MousePos: TPoint; var CanDock: Boolean);
begin
  with DockClient do
    CanDock := EnableDock and EachOtherDock and
      IsDockable(ParentForm, Client, Source.DropOnControl, Source.DropAlign);
end;

function TJvDockBasicStyle.DockClientWindowProc(DockClient: TJvDockClient; var Msg: TMessage): Boolean;
begin
  Result := False;
end;

procedure TJvDockBasicStyle.FormGetDockEdge(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject; MousePos: TPoint; var DropAlign: TAlign);
begin
  DropAlign := TControlAccessProtected(DockClient.ParentForm).GetDockEdge(MousePos);
end;

procedure TJvDockBasicStyle.SetConjoinServerOption(const Value: TJvDockBasicConjoinServerOption);
begin
  FConjoinServerOption.Assign(Value);
end;

procedure TJvDockBasicStyle.SetTabServerOption(const Value: TJvDockBasicTabServerOption);
begin
  FTabServerOption.Assign(Value);
end;

procedure TJvDockBasicStyle.CreateConjoinServerOption(var Option: TJvDockBasicConjoinServerOption);
begin
  Option := TJvDockBasicConjoinServerOption.Create(Self);
end;

procedure TJvDockBasicStyle.CreateTabServerOption(var Option: TJvDockBasicTabServerOption);
begin
  Option := TJvDockBasicTabServerOption.Create(Self);
end;

procedure TJvDockBasicStyle.CreateServerOption;
begin
  if FConjoinServerOption = nil then
    FConjoinServerOption := FConjoinServerOptionClass.Create(Self);
  if FTabServerOption = nil then
    FTabServerOption := FTabServerOptionClass.Create(Self);
end;

function TJvDockBasicStyle.GetConjoinServerOption: TJvDockBasicConjoinServerOption;
begin
  Result := FConjoinServerOption;
end;

function TJvDockBasicStyle.GetTabServerOption: TJvDockBasicTabServerOption;
begin
  Result := FTabServerOption;
end;

procedure TJvDockBasicStyle.FreeServerOption;
begin
  if FConjoinServerOption <> nil then
    FConjoinServerOption.Free;
  if FTabServerOption <> nil then
    FTabServerOption.Free;
end;

procedure TJvDockBasicStyle.AssignConjoinServerOption(APanel: TJvDockCustomPanel);
begin
  APanel.JvDockManager.GrabberSize := ConjoinServerOption.GrabbersSize;
  APanel.JvDockManager.SplitterWidth := ConjoinServerOption.SplitterWidth;
end;

procedure TJvDockBasicStyle.AssignTabServerOption(APage: TJvDockTabPageControl);
begin
  APage.HotTrack := TabServerOption.HotTrack;
  APage.TabPosition := TabServerOption.TabPosition;
end;

procedure TJvDockBasicStyle.Loaded;
begin
  inherited Loaded;
end;

procedure TJvDockBasicStyle.AfterConstruction;
begin
  inherited AfterConstruction;
  CreateServerOption;
end;

procedure TJvDockBasicStyle.ParentFormWindowProc(var Msg: TMessage);
begin
  if not (csDesigning in ComponentState) then
    if Assigned(FOldWindowProc) then
      FOldWindowProc(Msg);
end;

procedure TJvDockBasicStyle.AddDockBaseControl(ADockBaseControl: TJvDockBaseControl);
begin
  if ADockBaseControl = nil then
    Exit;
  if FDockBaseControlList.IndexOf(ADockBaseControl) = -1 then
  begin
    FDockBaseControlList.Add(ADockBaseControl);
    ConjoinServerOption.ResetDockControlOption;
    TabServerOption.ResetDockControlOption;
  end;
end;

procedure TJvDockBasicStyle.RemoveDockBaseControl(ADockBaseControl: TJvDockBaseControl);
begin
  if ADockBaseControl <> nil then
    FDockBaseControlList.Remove(ADockBaseControl);
end;

procedure TJvDockBasicStyle.ResetDockControlConjoinOption;
begin
end;

procedure TJvDockBasicStyle.ResetDockControlTabOption;
begin
end;

function TJvDockBasicStyle.GetCount: Integer;
begin
  Result := FDockBaseControlList.Count;
end;

function TJvDockBasicStyle.GetDockBaseControlLists(Index: Integer): TJvDockBaseControl;
begin
  Result := FDockBaseControlList[Index];
end;

function TJvDockBasicStyle.DockServerWindowProc(DockServer: TJvDockServer;
  var Msg: TMessage): Boolean;
begin
  Result := False;
end;

function TJvDockBasicStyle.GetDockState(DockClient: TJvDockClient): Integer;
begin
  Result := JvDockState_Unknown;
  if (DockClient <> nil) and (DockClient.ParentForm <> nil) then
    if DockClient.ParentForm.Floating then
      Result := JvDockState_Floating
    else
      Result := JvDockState_Docking;
end;

function TJvDockBasicStyle.CanSetBottomDocked(ADockBaseControl: TJvDockBaseControl): Boolean;
begin
  Result := True;
end;

function TJvDockBasicStyle.CanSetEachOtherDocked(ADockBaseControl: TJvDockBaseControl): Boolean;
begin
  Result := True;
end;

function TJvDockBasicStyle.CanSetEnableDocked(ADockBaseControl: TJvDockBaseControl): Boolean;
begin
  Result := True;
end;

function TJvDockBasicStyle.CanSetLeftDocked(ADockBaseControl: TJvDockBaseControl): Boolean;
begin
  Result := True;
end;

function TJvDockBasicStyle.CanSetRightDocked(ADockBaseControl: TJvDockBaseControl): Boolean;
begin
  Result := True;
end;

function TJvDockBasicStyle.CanSetTopDocked(ADockBaseControl: TJvDockBaseControl): Boolean;
begin
  Result := True;
end;

procedure TJvDockBasicStyle.ResetCursor(Source: TJvDockDragDockObject);
begin
  if (Source.TargetControl = nil) and (Source.Control <> nil) and (Source.Control.Floating) then
    Windows.SetCursor(Screen.Cursors[crDefault])
  else
  if (Source.TargetControl = nil) and (not JvGlobalDockClient.CanFloat) then
    Windows.SetCursor(Screen.Cursors[crNo])
  else
    Windows.SetCursor(Screen.Cursors[crDefault]);
end;

procedure TJvDockBasicStyle.HideDockForm(ADockClient: TJvDockClient);
begin
  if ADockClient <> nil then
  begin
    ADockClient.ParentForm.Visible := False;
    ADockClient.MakeHideEvent;
  end;
end;

procedure TJvDockBasicStyle.ShowDockForm(ADockClient: TJvDockClient);
begin
  if ADockClient <> nil then
  begin
    ADockClient.ParentForm.Visible := True;
    ADockClient.MakeShowEvent;
  end;
end;

function TJvDockBasicStyle.GetDockFormVisible(ADockClient: TJvDockClient): Boolean;
begin
  Result := True;
  if ADockClient <> nil then
  begin
    if ADockClient.ParentForm.Visible then
    begin
      if ADockClient.ParentForm.HostDockSite <> nil then
      begin
        if ADockClient.ParentForm.HostDockSite is TJvDockPanel then
          Result := ADockClient.ParentForm.HostDockSite.Width * ADockClient.ParentForm.HostDockSite.Height > 0
        else
          Result := GetFormVisible(ADockClient.ParentForm.HostDockSite.Parent);
      end;
    end
    else
      Result := False;
  end;
end;

procedure TJvDockBasicStyle.RestoreClient(DockClient: TJvDockClient);
var
  TmpLastDockSite: TWinControl;
  TmpUnDockLeft, TmpUnDockTop: Integer;
  I: Integer;
  ADockClient: TJvDockClient;
  ADockServer: TJvDockServer;
  ARect: TRect;

  procedure DoFloatParentForm;
  begin
    with DockClient do
      if (ParentForm.HostDockSite <> nil) then
      begin
        ARect := Bounds(TmpUnDockLeft, TmpUnDockTop, ParentForm.UndockWidth, ParentForm.UndockHeight);
        ParentForm.ManualFloat(ARect);
        if (ParentForm.Left <> ARect.Left) or (ParentForm.Top <> ARect.Top) then
        begin
          ParentForm.Left := ARect.Left;
          ParentForm.Top := ARect.Top;
        end;
      end;
  end;

begin
  if DockClient = nil then
    Exit;
  if not DockClient.CanFloat then
    Exit;
  with DockClient do
  begin
    if not EnableDock then
      Exit;
    if LastDockSite is TJvDockPanel then
    begin
      with TJvDockPanel(LastDockSite) do
      begin
        if ((not LeftDock) and (Align = alLeft)) or
          ((not RightDock) and (Align = alRight)) or
          ((not TopDock) and (Align = alTop)) or
          ((not BottomDock) and (Align = alBottom)) then
        begin
          DoFloatParentForm;
          Exit;
        end;

        ADockServer := DockServer;
        if ADockServer <> nil then
          if (not ADockServer.EnableDock) or
            ((not ADockServer.LeftDock) and (Align = alLeft)) or
            ((not ADockServer.RightDock) and (Align = alRight)) or
            ((not ADockServer.TopDock) and (Align = alTop)) or
            ((not ADockServer.BottomDock) and (Align = alBottom)) then
          begin
            DoFloatParentForm;
            Exit;
          end;
      end;
    end;

    if ParentForm is TJvDockConjoinHostForm then
    begin
      with TJvDockConjoinHostForm(ParentForm).Panel do
        for I := DockClientCount - 1 downto 0 do
        begin
          ADockClient := FindDockClient(DockClients[I]);
          if (ADockClient <> nil) and (ADockClient.LastDockSite is TJvDockPanel) then
            ADockClient.RestoreChild;
        end;
      Exit;
    end;

    TmpLastDockSite := LastDockSite;
    TmpUnDockLeft := UnDockLeft;
    TmpUnDockTop := UnDockTop;

    ResetDockClient(DockClient, nil);

    DoFloatParentForm;

    if TmpLastDockSite is TJvDockPanel then
    begin
      with TJvDockPanel(TmpLastDockSite) do
      begin
        if UseDockManager and (JvDockManager <> nil) then
        begin
          if not JvDockManager.HasZoneWithControl(ParentForm) then
            Exit;
          DisableAlign;
          try
            ParentForm.Dock(TmpLastDockSite, Rect(0, 0, 0, 0));

            JvDockManager.ShowControl(ParentForm);

            ParentForm.ActiveControl := nil;
            SetDockSite(ParentForm, False);

            if ParentForm.Visible and ParentForm.CanFocus then
              ParentForm.SetFocus;
            ShowDockPanel(True, ParentForm, sdfDockPanel);
          finally
            EnableAlign;
          end;
        end;
      end;
    end;
  end;
end;

//=== { TJvDockAdvStyle } ====================================================

function TJvDockAdvStyle.DockClientWindowProc(DockClient: TJvDockClient;
  var Msg: TMessage): Boolean;
begin
  if (DockClient <> nil) and (Msg.Msg = WM_NCLBUTTONDBLCLK) then
    if DockClient.CanFloat then
      DockClient.RestoreChild;
  Result := inherited DockClientWindowProc(DockClient, Msg);
end;

//=== { TJvDockSplitter } ====================================================

constructor TJvDockSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSnap := False;
end;

function TJvDockSplitter.FindControl: TControl;
begin
  if DockServer <> nil then
    Result := DockServer.GetDockPanelWithAlign(Align)
  else
    Result := inherited FindControl;
end;

function TJvDockSplitter.GetSplitterIndex: Integer;
begin
  case Align of
    alTop:
      Result := 0;
    alBottom:
      Result := 1;
    alLeft:
      Result := 2;
    alRight:
      Result := 3;
  else
    Result := -1;
  end;
end;

//=== { TJvDockBasicServerOption } ===========================================

constructor TJvDockBasicServerOption.Create(ADockStyle: TJvDockBasicStyle);
begin
  // (rom) added inherited Create
  inherited Create;
  FDockStyle := ADockStyle;
end;

procedure TJvDockBasicServerOption.Assign(Source: TPersistent);
begin
  if Source is TJvDockBasicServerOption then
  begin
    // TODO
  end
  else
    inherited Assign(Source);
end;

procedure TJvDockBasicServerOption.ResetDockClientOption(ADockClient: TJvDockClient);
begin
end;

procedure TJvDockBasicServerOption.ResetDockServerOption(ADockServer: TJvDockServer);
begin
end;

//=== { TJvDockBasicTabServerOption } ========================================

constructor TJvDockBasicTabServerOption.Create(ADockStyle: TJvDockBasicStyle);
begin
  inherited Create(ADockStyle);
  FHotTrack := False;
  FTabPosition := tpTop;
end;

procedure TJvDockBasicTabServerOption.Assign(Source: TPersistent);
begin
  if Source is TJvDockBasicTabServerOption then
  begin
    FTabPosition := TJvDockBasicTabServerOption(Source).TabPosition;
    FHotTrack := TJvDockBasicTabServerOption(Source).HotTrack;
  end
  else
    inherited Assign(Source);
end;

procedure TJvDockBasicTabServerOption.ResetDockClientOption(ADockClient: TJvDockClient);
var
  PageControl: TJvDockTabPageControl;
begin
  if ADockClient = nil then
    Exit;

  PageControl := TJvDockTabPageControl(TJvDockTabHostForm(ADockClient.ParentForm).PageControl);
  ResetTabPageControl(PageControl);
  if PageControl <> nil then
    PageControl.Invalidate;
end;

procedure TJvDockBasicTabServerOption.ResetDockControlOption;
var
  I: Integer;
  ADockClient: TJvDockClient;
begin
  for I := 0 to DockStyle.Count - 1 do
    if DockStyle.DockBaseControlLists[I] is TJvDockClient then
    begin
      ADockClient := TJvDockClient(DockStyle.DockBaseControlLists[I]);
      if ADockClient.ParentForm is TJvDockTabHostForm then
        ResetDockClientOption(ADockClient);
    end;
end;

procedure TJvDockBasicTabServerOption.ResetDockServerOption(ADockServer: TJvDockServer);
begin
end;

procedure TJvDockBasicTabServerOption.ResetTabPageControl(APage: TJvDockTabPageControl);
begin
  if APage <> nil then
  begin
    APage.HotTrack := FHotTrack;
    APage.TabPosition := FTabPosition;
  end;
end;

procedure TJvDockBasicTabServerOption.SetHotTrack(const Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    ResetDockControlOption;
  end;
end;

procedure TJvDockBasicTabServerOption.SetTabPosition(const Value: TTabPosition);
begin
  if FTabPosition <> Value then
  begin
    FTabPosition := Value;
    ResetDockControlOption;
  end;
end;

//=== { TJvDockBasicConjoinServerOption } ====================================

constructor TJvDockBasicConjoinServerOption.Create(ADockStyle: TJvDockBasicStyle);
begin
  inherited Create(ADockStyle);
  GrabbersSize := 12;
  SplitterWidth := 4;
end;

procedure TJvDockBasicConjoinServerOption.Assign(Source: TPersistent);
begin
  if Source is TJvDockBasicConjoinServerOption then
  begin
    FGrabbersSize := TJvDockBasicConjoinServerOption(Source).FGrabbersSize;
    FSplitterWidth := TJvDockBasicConjoinServerOption(Source).FSplitterWidth;
  end
  else
    inherited Assign(Source);
end;

procedure TJvDockBasicConjoinServerOption.ResetConjoinPanel(APanel: TJvDockConjoinPanel);
begin
  APanel.JvDockManager.GrabberSize := FGrabbersSize;
  APanel.JvDockManager.SplitterWidth := FSplitterWidth;
end;

procedure TJvDockBasicConjoinServerOption.ResetDockClientOption(ADockClient: TJvDockClient);
var
  ConjoinPanel: TJvDockConjoinPanel;
begin
  if ADockClient = nil then
    Exit;

  ConjoinPanel := TJvDockConjoinPanel(TJvDockConjoinHostForm(ADockClient.ParentForm).Panel);
  ResetConjoinPanel(ConjoinPanel);
  ConjoinPanel.Invalidate;
end;

procedure TJvDockBasicConjoinServerOption.ResetDockControlOption;
var
  I: Integer;
  ADockServer: TJvDockServer;
  ADockClient: TJvDockClient;
begin
  if DockStyle = nil then
    Exit;

  for I := 0 to DockStyle.Count - 1 do
    if DockStyle.DockBaseControlLists[I] is TJvDockServer then
    begin
      ADockServer := TJvDockServer(DockStyle.DockBaseControlLists[I]);
      ResetDockServerOption(ADockServer);
    end
    else
    if DockStyle.DockBaseControlLists[I] is TJvDockClient then
    begin
      ADockClient := TJvDockClient(DockStyle.DockBaseControlLists[I]);
      if ADockClient.ParentForm.HostDockSite is TJvDockConjoinPanel then
      begin
        ADockClient := FindDockClient(ADockClient.ParentForm.HostDockSite.Parent);
        ResetDockClientOption(ADockClient);
      end;
    end;
end;

procedure TJvDockBasicConjoinServerOption.ResetDockPanel(APanel: TJvDockPanel);
begin
  APanel.JvDockManager.GrabberSize := FGrabbersSize;
  APanel.JvDockManager.SplitterWidth := FSplitterWidth;
end;

procedure TJvDockBasicConjoinServerOption.ResetDockServerOption(ADockServer: TJvDockServer);
var
  I: TAlign;
begin
  if ADockServer = nil then
    Exit;
  for I := alTop to alRight do
  begin
    if ADockServer.DockPanelWithAlign[I] = nil then
      Break;
    ResetDockPanel(ADockServer.DockPanelWithAlign[I]);
    ADockServer.DockPanelWithAlign[I].Invalidate;
  end;
end;

procedure TJvDockBasicConjoinServerOption.SeTJvDockGrabbersSize(const Value: TJvDockGrabbersSize);
begin
  if FGrabbersSize <> Value then
  begin
    FGrabbersSize := Value;
    ResetDockControlOption;
  end;
end;

procedure TJvDockBasicConjoinServerOption.SetDockGrabbersSize_WithoutChangeSystemInfo(
  const Value: TJvDockGrabbersSize);
begin
  FGrabbersSize := Value;
end;

procedure TJvDockBasicConjoinServerOption.SetDockSplitterWidth(
  const Value: TJvDockSplitterWidth);
begin
  if FSplitterWidth <> Value then
  begin
    FSplitterWidth := Value;
    ResetDockControlOption;
  end;
end;

procedure TJvDockBasicConjoinServerOption.SetDockSplitterWidth_WithoutChangeSystemInfo(
  const Value: TJvDockSplitterWidth);
begin
  FSplitterWidth := Value;
end;

//=== { TJvDockAdvTabPageControl } ===========================================

destructor TJvDockAdvTabPageControl.Destroy;
var
  DockClient: TJvDockClient;
begin
  DockClient := FindDockClient(Parent);
  if (DockClient <> nil) and (DockClient.LastDockSite is TJvDockPanel) then
    with TJvDockPanel(DockClient.LastDockSite) do
      if UseDockManager and (JvDockManager <> nil) then
        JvDockManager.RemoveControl(Self.Parent);
  inherited Destroy;
end;

procedure TJvDockAdvTabPageControl.CMUnDockClient(var Msg: TCMUnDockClient);
begin
  inherited;
end;

procedure TJvDockAdvTabPageControl.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, Source.TargetControl);
  inherited CustomDockDrop(Source, X, Y);
end;

function TJvDockAdvTabPageControl.CustomUnDock(Source: TJvDockDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, NewTarget);
  Result := inherited CustomUnDock(Source, NewTarget, Client)
end;

procedure TJvDockAdvTabPageControl.DockDrop(Source: TDragDockObject;
  X, Y: Integer);
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, TControl(Source.DragTarget));
  inherited DockDrop(Source, X, Y);
end;

function TJvDockAdvTabPageControl.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Client, NewTarget);
  Result := inherited DoUnDock(NewTarget, Client);
end;

procedure TJvDockAdvConjoinPanel.CMUnDockClient(var Msg: TCMUnDockClient);
begin
  inherited;
end;

procedure TJvDockAdvConjoinPanel.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, Source.TargetControl);
  inherited CustomDockDrop(Source, X, Y);
end;

function TJvDockAdvConjoinPanel.CustomUnDock(Source: TJvDockDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, NewTarget);
  Result := inherited CustomUnDock(Source, NewTarget, Client);
end;

procedure TJvDockAdvConjoinPanel.DockDrop(Source: TDragDockObject;
  X, Y: Integer);
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, TControl(Source.DragTarget));
  inherited DockDrop(Source, X, Y);
end;

function TJvDockAdvConjoinPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Client, NewTarget);
  Result := inherited DoUnDock(NewTarget, Client);
end;

procedure InitDockManager;
var
  OSVersionInfo: TOSVersionInfo;
begin
  try
    JvGlobalDockManager.Free;
    JvGlobalDockManager := nil;
    JvGlobalDockManager := TJvDockManager.Create;

    OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
    GetVersionEx(OSVersionInfo);
    IsWinXP := (OSVersionInfo.dwMajorVersion = 5) and (OSVersionInfo.dwMinorVersion = 1);
  except
  end;
end;

procedure DoneDockManager;
begin
  JvGlobalDockManager.Free;
  JvGlobalDockManager := nil;
end;

initialization
  InitDockManager;

finalization
  DoneDockManager;

end.

