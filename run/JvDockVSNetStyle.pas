{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockVSNetStyle.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-12-31

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
}
{$I jvcl.inc}
unit JvDockVSNetStyle;

interface

uses Windows, Classes, Controls, Math, Messages, JvDockControlForm, JvDockSupportControl,
  JvDockTree, JvDockVIDStyle, Graphics, ComCtrls, Extctrls, ImgList, Forms;

const
  HTAUTOHIDE = 40;
  DefaultVSNETGrabberSize = 19;
  MaxActivePaneWidth = 100;
  VSNETPageInactiveFontColor = $00525552;
  VSNETPageInactiveSheetColor = $00EFF3F7;

type

  TJvDockVSNETConjoinServerOption = class(TJvDockVIDConjoinServerOption)
  protected
    procedure SetDefaultSystemCaptionInfo; override;
  public
    constructor Create(ADockStyle: TJvDockBasicStyle); override;
    destructor Destroy; override;
  end;

  TJvDockVSNETTabServerOption = class(TJvDockVIDTabServerOption)
  public
    constructor Create(ADockStyle: TJvDockBasicStyle); override;
  end;

  TJvDockVSNETChannelOption = class(TJvDockBasicServerOption)
  private
    FActivePaneSize: Integer;
    FShowImage: Boolean;
    FMouseleaveHide: Boolean;
    FHideHoldTime: Integer;
    procedure SetActivePaneSize(const Value: Integer);
    procedure SetShowImage(const Value: Boolean);
    procedure SetHideHoldTime(const Value: Integer);
    procedure SetMouseleaveHide(const Value: Boolean);
  protected
    procedure ResetDockControlOption; override;

    procedure ResetDockServerOption(ADockServer: TJvDockServer); override;

    procedure ResetDockClientOption(ADockClient: TJvDockClient); override;
  public
    constructor Create(ADockStyle: TJvDockBasicStyle); override;
  published
    property ActivePaneSize: Integer read FActivePaneSize write SetActivePaneSize;
    property ShowImage: Boolean read FShowImage write SetShowImage;
    property MouseleaveHide: Boolean read FMouseleaveHide write SetMouseleaveHide;
    property HideHoldTime: Integer read FHideHoldTime write SetHideHoldTime;
  end;

  TJvDockVSNETChannelOptionClass = class of TJvDockVSNETChannelOption;

  TJvDockVSBlock = class;
  TJvDockVSChannel = class;
  TJvDockVSNETPanel = class;
  TJvDockVSPopupPanel = class;
  TJvDockVSPopupPanelSplitter = class;

  TJvDockVSPane = class(TObject)
  public
    Block: TJvDockVSBlock;
    DockForm: TForm;
      Index: Integer;
    Width: Integer;
    Active: Boolean;
    Visible: Boolean;
    constructor Create(ABlock: TJvDockVSBlock; AForm: TForm; AWidth: Integer; AIndex: Integer); virtual;
    destructor Destroy; override;
  end;

  TJvDockBlockType = (btConjoinBlock, btTabBlock);

  TJvDockVSBlock = class(TObject)
  private
    FVSChannel: TJvDockVSChannel;
    FVSPaneList: TList;
    FActiveBlockWidth: Integer;
    FInactiveBlockWidth: Integer;
    FActiveDockControl: TWinControl;
    FBlockType: TJvDockBlockType;
    FImageList: TImageList;
    FBlockStartPos: Integer;
    function GetVSPane(Index: Integer): TJvDockVSPane;
    function GetCount: Integer;
  protected

    procedure ResetActiveBlockWidth;
    procedure DeletePane(Index: Integer);
    property ActiveBlockWidth: Integer read FActiveBlockWidth write FActiveBlockWidth;
    property InactiveBlockWidth: Integer read FInactiveBlockWidth write FInactiveBlockWidth;
    property ActiveDockControl: TWinControl read FActiveDockControl write FActiveDockControl;
    property BlockType: TJvDockBlockType read FBlockType;
    property VSChannel: TJvDockVSChannel read FVSChannel;
  public
    constructor Create(Owner: TJvDockVSChannel); virtual;
    destructor Destroy; override;
    procedure AddDockControl(Control: TWinControl);
    procedure RemoveDockControl(Control: TWinControl);
    function GetTotalWidth: Integer;
    property Count: Integer read GetCount;
    property VSPanes[Index: Integer]: TJvDockVSPane read GetVSPane;
  end;

  TJvDockChannelState = (csShow, csHide);

  TJvDockPoppupPanelAnimateStyle = (pasShow, pasHide);

  TJvDockVSChannel = class(TCustomControl)
  private
    FActiveDockForm: TForm;
    FActivePane: TJvDockVSPane;
    FVSNETDockPanel: TJvDockVSNETPanel;
    FCurrentPos: Integer;
    FDockServer: TJvDockServer;
    FBlockList: TList;
    FChannelWidth: Integer;
    FBlockStartOffset: Integer;
    FBlockUpOffset: Integer;
    FBlockInterval: Integer;
    FVSPopupPanel: TJvDockVSPopupPanel;
    FVSPopupPanelSplitter: TJvDockVSPopupPanelSplitter;
    FActivePaneSize: Integer;
    function GetCount: Integer;
    function GetBlocks(Index: Integer): TJvDockVSBlock;
    procedure GetBlockRect(Block: TJvDockVSBlock; Index: Integer; var ARect: TRect);
    function GetDockFormWithMousePos(MousePos: TPoint): TJvDockVSPane;
    procedure SetVSPopupPanelSplitter(const Value: TJvDockVSPopupPanelSplitter);
    procedure SetBlockStartOffset(const Value: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure FreeBlockList;
    procedure SetActivePaneSize(const Value: Integer);
  protected
    procedure ResetFontAngle; virtual;
    procedure ResetBlock; virtual;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure SetVSPopupPanelSplitterPosition;
    property ChannelWidth: Integer read FChannelWidth;
    property BlockStartOffset: Integer read FBlockStartOffset write SetBlockStartOffset;
    property BlockUpOffset: Integer read FBlockUpOffset;
    property BlockInterval: Integer read FBlockInterval;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetPaneWithControl(AControl: TControl): TJvDockVSPane;
    procedure CreateVSPopupPanel;
    procedure DestroyVSPopupPanel;
    procedure ResetPosition;
    procedure AddDockControl(Control: TWinControl);
    procedure RemoveDockControl(Control: TWinControl);
    function FindDockControl(Control: TWinControl; var BlockIndex: Integer;
      var PaneIndex: Integer): Boolean;
    function FindPane(Control: TWinControl): TJvDockVSPane;
    procedure PopupDockForm(Pane: TJvDockVSPane); overload;
    procedure PopupDockForm(Control: TWinControl); overload;
    procedure HidePopupPanel(Pane: TJvDockVSPane); overload;
    procedure HidePopupPanel(Control: TWinControl); overload;
    procedure HidePopupPanelWithAnimate;
    procedure ResetActivePaneWidth;
    procedure ResetPopupPanelHeight;
    procedure RemoveAllBlock;
    procedure DeleteBlock(Index: Integer);
    procedure AnimatePopupPanel(AnimateStyle: TJvDockPoppupPanelAnimateStyle);
    property DockServer: TJvDockServer read FDockServer write FDockServer;
    property Count: Integer read GetCount;
    property Blocks[Index: Integer]: TJvDockVSBlock read GetBlocks;
    property VSPopupPanel: TJvDockVSPopupPanel read FVSPopupPanel;
    property VSPopupPanelSplitter: TJvDockVSPopupPanelSplitter read FVSPopupPanelSplitter
      write SetVSPopupPanelSplitter;
    property ActiveDockForm: TForm read FActiveDockForm;
    property ActivePaneSize: Integer read FActivePaneSize write SetActivePaneSize;
  end;

  TJvDockVSChannelClass = class of TJvDockVSChannel;

  TJvDockVSNetStyle = class(TJvDockVIDStyle)
  private
    FTimer: TTimer;
    FDockServerList: TList;
    FCurrentTimer: Integer;
    FChannelOption: TJvDockVSNETChannelOption;
    FChannelOptionClass: TJvDockVSNETChannelOptionClass;
    procedure TimerTimer(Sender: TObject);

    procedure SetChannelOption(const Value: TJvDockVSNETChannelOption);
    function GetChannelOption: TJvDockVSNETChannelOption;

  protected
    procedure CreateConjoinServerOption(var Option: TJvDockBasicConjoinServerOption); override;
    procedure CreateTabServerOption(var Option: TJvDockBasicTabServerOption); override;

    function DockServerWindowProc(DockServer: TJvDockServer; var Message: TMessage): Boolean; override;

    function DockClientWindowProc(DockClient: TJvDockClient; var Message: TMessage): Boolean; override;

    procedure AddDockBaseControl(ADockBaseControl: TJvDockBaseControl); override;
    procedure CreateServerOption; override;
    procedure FreeServerOption; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFNDEF USEJVCL}
    function GetControlName: string; override;
{$ENDIF}

    procedure SetDockFormVisible(ADockClient: TJvDockClient; AVisible: Boolean);
    procedure ShowDockForm(ADockClient: TJvDockClient); override;
    procedure HideDockForm(ADockClient: TJvDockClient); override;
    function GetDockFormVisible(ADockClient: TJvDockClient): Boolean; override;
    procedure RestoreClient(DockClient: TJvDockClient); override;

    class procedure SetAnimationInterval(const Value:integer);
    class function GetAnimationInterval:integer;
    class procedure SetAnimationMoveWidth(const Value:integer);
    class function GetAnimationMoveWidth:integer;

  published
    property ChannelOption: TJvDockVSNETChannelOption read GetChannelOption write SetChannelOption;
  end;

  TJvDockVSNETSplitter = class(TJvDockVIDSplitter);

  TJvDockVSNETPanel = class(TJvDockVIDPanel)
  private
    FVSChannelClass: TJvDockVSChannelClass;
    FVSChannel: TJvDockVSChannel;
  protected
    procedure SetDockServer(const Value: TJvDockServer); override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateVSChannel;
    procedure DestroyVSChannel;
    procedure DoAutoHideControl(Control: TWinControl);
    procedure DoHideControl(Control: TWinControl);
    procedure DoShowControl(Control: TWinControl);
    property VSChannel: TJvDockVSChannel read FVSChannel;
  end;

  TJvDockVSPopupPanel = class(TJvDockVSNETPanel)
  private
    FVSNETDockPanel: TJvDockVSNETPanel;
    procedure SetVSNETDockPanel(const Value: TJvDockVSNETPanel);
    function GetVSChannel: TJvDockVSChannel;
  protected
    function CreateDockManager: IDockManager; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowDockPanel(MakeVisible: Boolean; Client: TControl;
      PanelSizeFrom: TJvDockSetDockPanelSizeFrom); override;
    property VSChannel: TJvDockVSChannel read GetVSChannel;
    property VSNETDockPanel: TJvDockVSNETPanel read FVSNETDockPanel
      write SetVSNETDockPanel;
  end;

  TJvDockVSNETConjoinPanel = class(TJvDockVIDConjoinPanel);

  TJvDockBtnState = (bsUp, bsNormal, bsDown);

  TJvDockVSNETZone = class(TJvDockVIDZone)
  private
    FAutoHideButtonDown: Boolean;
    FAutoHideButtonState: TJvDockBtnState;
    FCloseButtonState: TJvDockBtnState;
    FVSPaneVisible: Boolean;
    procedure SetAutoHideButtonState(const Value: TJvDockBtnState);
    procedure SetCloseButtonState(const Value: TJvDockBtnState);
    procedure SetAutoHideButtonDown(const Value: Boolean);
    procedure SetVSPaneVisible(const Value: Boolean);
  protected
    procedure DoCustomSetControlName; override;
    procedure SetChildControlVisible(Client: TControl; AViisible: Boolean); override;
    property AutoHideButtonDown: Boolean read FAutoHideButtonDown write SetAutoHideButtonDown;
    property AutoHideButtonState: TJvDockBtnState read FAutoHideButtonState write SetAutoHideButtonState;
    property CloseButtonState: TJvDockBtnState read FCloseButtonState write SetCloseButtonState;
    property VSPaneVisible: Boolean read FVSPaneVisible write SetVSPaneVisible;
  public
    constructor Create(Tree: TJvDockTree); override;
  end;

  TJvDockVSNETTree = class(TJvDockVIDTree)
  private
    FAutoHideZone: TJvDockVSNETZone;
  protected
    procedure IgnoreZoneInfor(Stream: TMemoryStream); override;
    procedure BeginDrag(Control: TControl;
      Immediate: Boolean; Threshold: Integer = -1); override;
    function DoLButtonDown(var Message: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer): Boolean; override;
    procedure DoLButtonUp(var Message: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer); override;
    procedure DoLButtonDbClk(var Message: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer); override;
    procedure DoMouseMove(var Message: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer); override;

    procedure DoHideZoneChild(AZone: TJvDockZone); override;
    function GetTopGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TJvDockZone): TJvDockZone; override;
    procedure DrawDockGrabber(Control: TControl; const ARect: TRect); override;
    procedure PaintDockGrabberRect(Canvas: TCanvas; Control: TControl;
      const ARect: TRect); override;
    procedure DrawCloseButton(Canvas: TCanvas; Zone: TJvDockZone;
      Left, Top: Integer); override;
    procedure DrawAutoHideButton(Zone: TJvDockZone;
      Left, Top: Integer); virtual;
    procedure GetCaptionRect(var Rect: TRect); override;

    procedure DoOtherHint(Zone: TJvDockZone;
      HTFlag: Integer; var HintStr: string); override;
    procedure CustomSaveZone(Stream: TStream;
      Zone: TJvDockZone); override;
    procedure CustomLoadZone(Stream: TStream;
      var Zone: TJvDockZone); override;
    property AutoHideZone: TJvDockVSNETZone read FAutoHideZone
      write FAutoHideZone;
  public
    constructor Create(DockSite: TWinControl;
      DockZoneClass: TJvDockZoneClass); override;
    destructor Destroy; override;
  end;

  TJvDockVSNETTabSheet = class(TJvDockVIDTabSheet)
  private
    FPreviousVisible: Boolean;
    procedure SetPreviousVisible(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    property PreviousVisible: Boolean read FPreviousVisible write SetPreviousVisible;
  end;

  TJvDockVSNETTabPanel = class(TJvDockTabPanel)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvDockVSNETTabPageControl = class(TJvDockVIDTabPageControl)
  protected
    procedure CreatePanel; override;
    procedure ShowControl(AControl: TControl); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvDockVSNETDragDockObject = class(TJvDockVIDDragDockObject);

  TJvDockVSPopupPanelSplitter = class(TCustomControl)
  private
    FVSPopupPanel: TJvDockVSPopupPanel;
    FSplitWidth: Integer;
    FActiveControl: TWinControl;
    FAutoSnap: Boolean;
    FBeveled: Boolean;
    FBrush: TBrush;
    FControl: TControl;
    FDownPos: TPoint;
    FLineDC: HDC;
    FLineVisible: Boolean;
    FMinSize: NaturalNumber;
    FMaxSize: Integer;
    FNewSize: Integer;
    FOldKeyDown: TKeyEvent;
    FOldSize: Integer;
    FPrevBrush: HBrush;
    FResizeStyle: TResizeStyle;
    FSplit: Integer;
    FOnCanResize: TCanResizeEvent;
    FOnMoved: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    procedure AllocateLineDC;
    procedure CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    procedure DrawLine;
    function FindControl: TControl;
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ReleaseLineDC;
    procedure SetBeveled(Value: Boolean);
    procedure UpdateControlSize;
    procedure UpdateSize(X, Y: Integer);
    procedure SetVSPopupPanel(const Value: TJvDockVSPopupPanel);
    function GetVSChannelAlign: TAlign;
    procedure SetSplitWidth(const Value: Integer);
  protected
    function CanResize(var NewSize: Integer): Boolean; reintroduce; virtual;
    function DoCanResize(var NewSize: Integer): Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure RequestAlign; override;
    procedure StopSizing; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property VSPopupPanel: TJvDockVSPopupPanel read FVSPopupPanel write SetVSPopupPanel;
    property SplitWidth: Integer read FSplitWidth write SetSplitWidth;
  published
    property Align default alLeft;
    property VSChannelAlign: TAlign read GetVSChannelAlign;
    property AutoSnap: Boolean read FAutoSnap write FAutoSnap default True;
    property Beveled: Boolean read FBeveled write SetBeveled default False;
    property Color;
    property Constraints;
    property MinSize: NaturalNumber read FMinSize write FMinSize default 30;
    property ParentColor;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle
      default rsPattern;
    property Visible;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

procedure HideAllPopupPanel(ExcludeChannel: TJvDockVSChannel);

var
  DefaultVSChannelClass: TJvDockVSChannelClass = nil;

implementation

uses SysUtils, JvDockSupportProc, JvDockGlobals, Dialogs, AppEvnts;

type
  TAnimateState = (asPopup, asHide);

  TPopupPanelAnimate = class(TTimer)
  private
    FMaxWidth: Integer;
    FCurrentWidth: Integer;
    FVSChannel: TJvDockVSChannel;
    FState: TAnimateState;
  protected
    procedure Timer; override;
    procedure OnCustomTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PopupForm(VSChannel: TJvDockVSChannel; MaxWidth: Integer); virtual;
    procedure HideForm(VSChannel: TJvDockVSChannel; MaxWidth: Integer); virtual;
  end;

  TJvDockAppEvents = class(TApplicationEvents)
  private
    FOldOnMessage: TMessageEvent;
    procedure NewOnMessage(var Msg: TMsg; var Handled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  FPopupPanelAnimate: TPopupPanelAnimate = nil;
  FApplicationEvents: TJvDockAppEvents = nil;
  FPopupPanelAnimateInterval: Integer = 20;
  FPopupPanelAnimateMoveWidth: Integer = 20;

  // (p3) not used:
//  AnimateSleepTime: Integer = 500;

function PopupPanelAnimate:TPopupPanelAnimate;
begin
  if FPopupPanelAnimate = nil then
    FPopupPanelAnimate := TPopupPanelAnimate.Create(nil);
  Result := FPopupPanelAnimate;
end;

function ApplicationEvents:TJvDockAppEvents;
begin
  if FApplicationEvents = nil then
    FApplicationEvents := TJvDockAppEvents.Create(nil);
  Result := FApplicationEvents;
end;


procedure HideAllPopupPanel(ExcludeChannel: TJvDockVSChannel);
var
  i, j: Integer;
  Channel: TJvDockVSChannel;
  DockServer: TJvDockServer;
begin

  for i := 0 to JvGlobalDockManager.DockServersList.Count - 1 do
  begin
    DockServer := FindDockServer(JvGlobalDockManager.DockServersList[i]);
    if (DockServer <> nil) and (DockServer.DockPanel[0] is TJvDockVSNETPanel) then
      for j := 0 to 3 do
      begin
        Channel := TJvDockVSNETPanel(DockServer.DockPanel[j]).VSChannel;
        if (Channel <> nil) and (Channel <> ExcludeChannel) then
          Channel.HidePopupPanel(Channel.FActivePane);
      end;
  end;
end;

procedure ResetChannelBlockStartOffset(Channel: TJvDockVSChannel);
var
  i: Integer;
  LeftChannel: TJvDockVSChannel;
  CurrChannel: TJvDockVSChannel;
  OldOffset: Integer;
  LeftAlignArea: Integer;
begin
  LeftChannel := TJvDockVSNETPanel(Channel.DockServer.LeftDockPanel).VSChannel;
  if (LeftChannel <> nil) then
  begin
    LeftAlignArea := GetClientAlignControlArea(LeftChannel.Parent, alLeft);
    for i := 0 to 3 do
    begin
      CurrChannel := TJvDockVSNETPanel(Channel.DockServer.DockPanel[i]).VSChannel;
      if (CurrChannel.Align in [alTop, alBottom]) then
      begin
        OldOffset := CurrChannel.BlockStartOffset;
        CurrChannel.BlockStartOffset := 2 + LeftAlignArea;
        if OldOffset <> CurrChannel.BlockStartOffset then
          CurrChannel.Invalidate;
      end;
    end;
  end;
end;

procedure TJvDockVSNetStyle.AddDockBaseControl(
  ADockBaseControl: TJvDockBaseControl);
begin
  if ADockBaseControl = nil then Exit;
  if DockBaseControlList.IndexOf(ADockBaseControl) = -1 then
  begin
    inherited;
    ChannelOption.ResetDockControlOption;
  end;

  if (ADockBaseControl is TJvDockServer) then
  begin
    if FDockServerList.IndexOf(ADockBaseControl) = -1 then
      FDockServerList.Add(ADockBaseControl);
    if not Assigned(FTimer) then
    begin
      FTimer := TTimer.Create(self);
      FTimer.Interval := 100;
      FTimer.OnTimer := self.TimerTimer;
      FTimer.Enabled := True;
    end;
  end;

end;

constructor TJvDockVSNetStyle.Create(AOwner: TComponent);
begin
  inherited;
  DockPanelClass := TJvDockVSNETPanel;
  DockSplitterClass := TJvDockVSNETSplitter;
  ConjoinPanelClass := TJvDockVSNETConjoinPanel;
  TabDockClass := TJvDockVSNETTabPageControl;
  DockPanelTreeClass := TJvDockVSNETTree;
  DockPanelZoneClass := TJvDockVSNETZone;
  ConjoinPanelTreeClass := TJvDockVSNETTree;
  ConjoinPanelZoneClass := TJvDockVSNETZone;
  ConjoinServerOptionClass := TJvDockVSNETConjoinServerOption;
  TabServerOptionClass := TJvDockVSNETTabServerOption;
  FChannelOptionClass := TJvDockVSNETChannelOption;

  FDockServerList := TList.Create;
end;

procedure TJvDockVSNetStyle.CreateConjoinServerOption(
  var Option: TJvDockBasicConjoinServerOption);
begin
  Option := TJvDockVSNETConjoinServerOption.Create(Self);
end;

procedure TJvDockVSNetStyle.CreateServerOption;
begin
  inherited;
  if FChannelOptionClass <> nil then
    FChannelOption := FChannelOptionClass.Create(Self);
end;

procedure TJvDockVSNetStyle.CreateTabServerOption(
  var Option: TJvDockBasicTabServerOption);
begin
  Option := TJvDockVSNETTabServerOption.Create(Self);
end;

destructor TJvDockVSNetStyle.Destroy;
begin

  if FTimer <> nil then
    FTimer.Free;
  if FDockServerList <> nil then
    FDockServerList.Free;

  inherited;
end;

function TJvDockVSNetStyle.DockClientWindowProc(DockClient: TJvDockClient;
  var Message: TMessage): Boolean;
var
  Channel: TJvDockVSChannel;
begin
  Result := inherited DockClientWindowProc(DockClient, Message);
  if (Message.Msg = CM_ENTER) or (Message.Msg = CM_EXIT) then
  begin
    Channel := nil;
    if (DockClient.ParentForm.HostDockSite is TJvDockVSPopupPanel) then
      Channel := TJvDockVSPopupPanel(DockClient.ParentForm.HostDockSite).VSChannel
    else if DockClient.ParentForm.HostDockSite <> nil then
    begin
      if (DockClient.ParentForm.HostDockSite.Parent is TJvDockVSPopupPanel) then
        Channel := TJvDockVSPopupPanel(DockClient.ParentForm.HostDockSite.Parent).VSChannel
      else if (DockClient.ParentForm.HostDockSite.Parent <> nil)
        and (DockClient.ParentForm.HostDockSite.Parent.Parent is TJvDockVSPopupPanel) then
        Channel := TJvDockVSPopupPanel(DockClient.ParentForm.HostDockSite.Parent.Parent).VSChannel;
    end;
    if (Message.Msg = CM_EXIT) then
    begin
      if Channel <> nil then
        Channel.HidePopupPanelWithAnimate;
    end
    else if (Message.Msg = CM_ENTER) then
    begin
      HideAllPopupPanel(Channel);
    end;
  end;
end;

function TJvDockVSNetStyle.DockServerWindowProc(DockServer: TJvDockServer;
  var Message: TMessage): Boolean;
var
  i: Integer;
  Channel: TJvDockVSChannel;
begin
  Result := inherited DockServerWindowProc(DockServer, Message);
  if (Message.Msg = WM_SIZE) then
  begin
    for i := 0 to 3 do
    begin
      Channel := nil;
      if DockServer.DockPanel[i] <> nil then
        Channel := TJvDockVSNETPanel(DockServer.DockPanel[i]).VSChannel;
      if Channel <> nil then
        Channel.HidePopupPanel(Channel.FActivePane);
    end;
  end;
end;

procedure TJvDockVSNetStyle.FreeServerOption;
begin
  inherited;
  if FChannelOption <> nil then
    FChannelOption.Free;
end;

function TJvDockVSNetStyle.GetChannelOption: TJvDockVSNETChannelOption;
begin
  Result := FChannelOption;
end;

{$IFNDEF USEJVCL}
function TJvDockVSNetStyle.GetControlName: string;
begin
  Result := Format(RsDockLikeVSNETStyle, [RsDockStyleName]);
end;
{$ENDIF}

function TJvDockVSNetStyle.GetDockFormVisible(
  ADockClient: TJvDockClient): Boolean;
var
  VSChannel: TJvDockVSChannel;
  Pane: TJvDockVSPane;
begin
  Result := True;
  if ADockClient <> nil then
  begin
    if not (ADockClient.ParentForm is TJvDockTabHostForm) and
      (ADockClient.ParentForm.HostDockSite is TJvDockVSPopupPanel) then
    begin

      VSChannel := TJvDockVSPopupPanel(ADockClient.ParentForm.HostDockSite).VSChannel;
      if VSChannel <> nil then
        Pane := VSChannel.FindPane(ADockClient.ParentForm)
      else
        Pane := nil;
      if Pane <> nil then
        Result := Pane.Visible;
    end
    else if (ADockClient.ParentForm.HostDockSite <> nil) and (ADockClient.ParentForm.HostDockSite.Parent <> nil) and
      (ADockClient.ParentForm.HostDockSite.Parent.HostDockSite is TJvDockVSPopupPanel) then
    begin

      VSChannel := TJvDockVSPopupPanel(ADockClient.ParentForm.HostDockSite.Parent.HostDockSite).VSChannel;
      if VSChannel <> nil then
        Pane := VSChannel.FindPane(ADockClient.ParentForm)
      else
        Pane := nil;
      if Pane <> nil then
        Result := Pane.Visible;
    end
    else
      Result := inherited GetDockFormVisible(ADockClient);
  end;
end;

procedure TJvDockVSNetStyle.HideDockForm(ADockClient: TJvDockClient);
begin
  inherited;
  SetDockFormVisible(ADockClient, False);
end;

procedure TJvDockVSNetStyle.RestoreClient(DockClient: TJvDockClient);
begin
  if (DockClient.ParentForm.HostDockSite is TJvDockVSPopupPanel) or
    ((DockClient.ParentForm.Parent <> nil) and (DockClient.ParentForm.Parent.HostDockSite is TJvDockVSPopupPanel)) then
    Exit;
  inherited;
end;

procedure TJvDockVSNetStyle.SetChannelOption(const Value: TJvDockVSNETChannelOption);
begin
  FChannelOption.Assign(Value);
end;

procedure TJvDockVSNetStyle.SetDockFormVisible(ADockClient: TJvDockClient;
  AVisible: Boolean);
var
  VSChannel: TJvDockVSChannel;
  Pane: TJvDockVSPane;

  procedure ResetActiveControl;
  var
    i: Integer;
  begin
    if AVisible then
      Pane.Block.ActiveDockControl := ADockClient.ParentForm
    else
    begin
      for i := Pane.Index downto 0 do
      begin
        if Pane.Block.VSPanes[i].Visible then
        begin
          Pane.Block.ActiveDockControl := Pane.Block.VSPanes[i].DockForm;
          Exit;
        end;
      end;
      for i := Pane.Index + 1 to Pane.Block.Count - 1 do
      begin
        if Pane.Block.VSPanes[i].Visible then
        begin
          Pane.Block.ActiveDockControl := Pane.Block.VSPanes[i].DockForm;
          Exit;
        end;
      end;
    end;
  end;
begin
  if (ADockClient <> nil) then
  begin
    VSChannel := nil;
    if not (ADockClient.ParentForm is TJvDockTabHostForm) and
      (ADockClient.ParentForm.HostDockSite is TJvDockVSPopupPanel) then
    begin
      VSChannel := TJvDockVSPopupPanel(ADockClient.ParentForm.HostDockSite).VSChannel;
      if VSChannel <> nil then
        Pane := VSChannel.FindPane(ADockClient.ParentForm)
      else
        Pane := nil;
      Pane := VSChannel.FindPane(ADockClient.ParentForm);
      if Pane <> nil then
      begin
        Pane.Visible := AVisible;
        ResetActiveControl;
      end;
    end
    else if (ADockClient.ParentForm.HostDockSite <> nil) and (ADockClient.ParentForm.HostDockSite.Parent <> nil) and
      (ADockClient.ParentForm.HostDockSite.Parent.HostDockSite is TJvDockVSPopupPanel) then
    begin
      VSChannel := TJvDockVSPopupPanel(ADockClient.ParentForm.HostDockSite.Parent.HostDockSite).VSChannel;
      if VSChannel <> nil then
        Pane := VSChannel.FindPane(ADockClient.ParentForm)
      else
        Pane := nil;
      Pane := VSChannel.FindPane(ADockClient.ParentForm);
      if Pane <> nil then
      begin
        Pane.Visible := AVisible;
        ResetActiveControl;
        TJvDockVSNETTabSheet(ADockClient.ParentForm.Parent).PreviousVisible := AVisible;
      end;
    end;
    if VSChannel <> nil then
    begin
      VSChannel.ResetPosition;
      VSChannel.Invalidate;
    end;
  end;
end;

procedure TJvDockVSNetStyle.ShowDockForm(ADockClient: TJvDockClient);
begin
  inherited;
  SetDockFormVisible(ADockClient, True);
end;

procedure TJvDockVSNetStyle.TimerTimer(Sender: TObject);
var
  MPosTp: Tpoint;
  ControlH: HWND;
  WControl: TWinControl;
  p: TWinControl;
  i, j: integer;
  VSChannel: TJvDockVSChannel;
begin
  if not ChannelOption.MouseleaveHide then Exit;
  if (csDesigning in ComponentState) then exit;

  if (GetAsyncKeyState(VK_LBUTTON) and $8000) <> 0 then
    Exit;
  GetCursorPos(MPosTp);
  ControlH := WindowFromPoint(MPosTp);
  WControl := FindControl(ControlH);
  p := WControl;
  while p <> nil do
  begin
    if (p is TJvDockVSPopupPanel) or
      (p is TJvDockVSPopupPanelSplitter) or
      (p is TJvDockVSChannel) then
    begin
      FCurrentTimer := ChannelOption.HideHoldTime;
      break;
    end;
    p := p.Parent;
  end;
  if p = nil then
  begin
    Dec(FCurrentTimer, 100);
    if (FCurrentTimer > 0) or (FCurrentTimer < -100) then
      Exit
    else
      FCurrentTimer := -101;
    for i := 0 to FDockServerList.Count - 1 do
      for j := 0 to 3 do
      begin
        VSChannel := TJvDockVSNETPanel(TJvDockServer(FDockServerList[i]).DockPanel[j]).VSChannel;
        VSChannel.HidePopupPanelWithAnimate;
      end;
  end;
end;

class function TJvDockVSNetStyle.GetAnimationInterval: integer;
begin
  Result := FPopupPanelAnimateInterval;
end;

class function TJvDockVSNetStyle.GetAnimationMoveWidth: integer;
begin
  Result := FPopupPanelAnimateMoveWidth;
end;

class procedure TJvDockVSNetStyle.SetAnimationInterval(const Value: integer);
begin
  if FPopupPanelAnimateInterval <> Value then
  begin
    FPopupPanelAnimateInterval := Value;
    FreeAndNil(FPopupPanelAnimate);
  end;
end;

class procedure TJvDockVSNetStyle.SetAnimationMoveWidth(const Value: integer);
begin
  if FPopupPanelAnimateMoveWidth <> Value then
  begin
    FPopupPanelAnimateMoveWidth := Value;
    FreeAndNil(FPopupPanelAnimate);
  end;
end;

procedure TJvDockVSNETTree.BeginDrag(Control: TControl; Immediate: Boolean;
  Threshold: Integer);
begin
  if not (DockSite is TJvDockVSPopupPanel) then
    inherited;
end;

constructor TJvDockVSNETTree.Create(DockSite: TWinControl;
  DockZoneClass: TJvDockZoneClass);
begin
  inherited;
  GrabberSize := DefaultVSNETGrabberSize;
  ButtonHeight := 12;
  ButtonWidth := 16;
  LeftOffset := 2;
  RightOffset := 3;
  TopOffset := 4;
  BottomOffset := 3;
  ButtonSplitter := 2;
  CaptionLeftOffset := 5;
  CaptionRightOffset := 5;
end;

procedure TJvDockVSNETTree.CustomLoadZone(Stream: TStream;
  var Zone: TJvDockZone);
var
  Pane: TJvDockVSPane;
  i: Integer;

  procedure SetPaneVisible(ChildControl: TControl; VSPaneVisible: Boolean);
  var
    DockClient: TJvDockClient;
  begin
    if (Pane <> nil) then
    begin
      Pane.Visible := VSPaneVisible;
      DockClient := FindDockClient(Pane.DockForm);
      if DockClient <> nil then
      begin
        if Pane.Visible then
        begin
          DockClient.ParentVisible := False;
          DockClient.ParentForm.Visible := True;
          DockClient.MakeShowEvent;
        end
        else
          DockClient.MakeHideEvent;
      end;
    end;
  end;

var
  Sheet: TJvDockVSNETTabSheet;

begin
  inherited CustomLoadZone(Stream, Zone);
  Stream.Read(TJvDockVSNETZone(Zone).FVSPaneVisible, SizeOf(TJvDockVSNETZone(Zone).VSPaneVisible));
  if DockSite is TJvDockVSPopupPanel then
  begin
    with TJvDockVSPopupPanel(DockSite).VSChannel, TJvDockVSNETZone(Zone) do
    begin
      if ChildControl is TJvDockTabHostForm then
      begin
        for i := 0 to TJvDockTabHostForm(ChildControl).PageControl.Count - 1 do
        begin
          Sheet := TJvDockVSNETTabSheet(TJvDockTabHostForm(ChildControl).PageControl.Pages[i]);
          Pane := FindPane(TWinControl(Sheet.Controls[0]));
          SetPaneVisible(ChildControl, Sheet.PreviousVisible);
        end;
      end
      else
      begin
        Pane := FindPane(ChildControl);
        SetPaneVisible(ChildControl, VSPaneVisible);
      end;
      ResetPosition;
    end;
  end;
end;

procedure TJvDockVSNETTree.CustomSaveZone(Stream: TStream;
  Zone: TJvDockZone);
var
  Pane: TJvDockVSPane;
begin
  inherited CustomSaveZone(Stream, Zone);
  if DockSite is TJvDockVSPopupPanel then
  begin
    with TJvDockVSPopupPanel(DockSite).VSChannel, TJvDockVSNETZone(Zone) do
    begin
      Pane := FindPane(ChildControl);
      if (Pane <> nil) then
        VSPaneVisible := Pane.Visible;
    end;
  end;
  Stream.Write(TJvDockVSNETZone(Zone).VSPaneVisible, SizeOf(TJvDockVSNETZone(Zone).VSPaneVisible));
end;

destructor TJvDockVSNETTree.Destroy;
begin
  inherited;

end;

function TJvDockVSNETTree.DoLButtonDown(var Message: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer): Boolean;
begin
  Result := inherited DoLButtonDown(Message, Zone, HTFlag);
  if (Zone <> nil) then
  begin
    if (HTFlag = HTCLOSE) then
      TJvDockVSNETZone(Zone).CloseButtonState := bsDown
    else if HTFlag = HTAUTOHIDE then
    begin
      AutoHideZone := TJvDockVSNETZone(Zone);
      AutoHideZone.AutoHideButtonDown := True;
      AutoHideZone.AutoHideButtonState := bsDown;
    end;
  end;
end;

procedure TJvDockVSNETTree.DoLButtonUp(var Message: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer);
begin
  if CloseButtonZone <> nil then
  begin
    TJvDockVSNETZone(CloseButtonZone).CloseButtonState := bsNormal;
  end;
  inherited;
  if (AutoHideZone <> nil) then
  begin
    AutoHideZone.AutoHideButtonDown := False;
    AutoHideZone.AutoHideButtonState := bsNormal;
    if HTFlag = HTAUTOHIDE then
    begin
      if DockSite is TJvDockVSNETPanel then
        TJvDockVSNETPanel(DockSite).DoAutoHideControl(AutoHideZone.ChildControl);
    end;
    AutoHideZone := nil;
  end;
end;

procedure TJvDockVSNETTree.DoMouseMove(var Message: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer);
var
  AZone: TJvDockVSNETZone;
begin
  inherited;
  if Zone <> nil then
  begin
    AZone := TJvDockVSNETZone(Zone);
    if AZone.AutoHideButtonDown then
    begin
      if HTFlag = HTAUTOHIDE then
        AZone.AutoHideButtonState := bsDown
      else
        AZone.AutoHideButtonState := bsUp;
    end
    else if (HTFlag = HTAUTOHIDE) and not AZone.CloseBtnDown then
      AZone.AutoHideButtonState := bsUp
    else
      AZone.AutoHideButtonState := bsNormal;

    if AZone.CloseBtnDown then
    begin
      if HTFlag = HTCLOSE then
        AZone.CloseButtonState := bsDown
      else
        AZone.CloseButtonState := bsUp;
    end
    else if (HTFlag = HTCLOSE) and not AZone.AutoHideButtonDown then
      AZone.CloseButtonState := bsUp
    else
      AZone.CloseButtonState := bsNormal;
  end;
end;

procedure TJvDockVSNETTree.DoOtherHint(Zone: TJvDockZone; HTFlag: Integer;
  var HintStr: string);
begin
  inherited;
  if (HTFlag = HTAUTOHIDE) then
    HintStr := RsDockVSNETDockTreeAutoHideBtnHint;
end;

procedure TJvDockVSNETTree.DrawAutoHideButton(
  Zone: TJvDockZone; Left, Top: Integer);
var
  AZone: TJvDockVSNETZone;
  ColorArr: array[1..2] of TColor;
begin
  if Zone <> nil then
  begin
    AZone := TJvDockVSNETZone(Zone);

    if AZone.AutoHideButtonState <> bsNormal then
    begin
      if AZone.AutoHideButtonState = bsUp then
      begin
        ColorArr[1] := clBlack;
        if GetActiveControl = AZone.ChildControl then
          ColorArr[2] := clBtnface
        else
          ColorArr[2] := clWhite;
      end
      else if AZone.AutoHideButtonState = bsDown then
      begin
        ColorArr[1] := clBtnface;
        ColorArr[2] := clBlack;
      end;
      Canvas.Pen.Color := ColorArr[1];
      Canvas.MoveTo(Left, Top + ButtonHeight);
      Canvas.LineTo(Left + ButtonWidth, Top + ButtonHeight);
      Canvas.LineTo(Left + ButtonWidth, Top);
      Canvas.Pen.Color := ColorArr[2];
      Canvas.LineTo(Left, Top);
      Canvas.LineTo(Left, Top + ButtonHeight);
    end;

    if AZone.AutoHideButtonState = bsDown then
    begin
      Inc(Left);
      Inc(Top);
    end;

    if AZone.ChildControl = GetActiveControl then
      Canvas.Pen.Color := clWhite
    else
      Canvas.Pen.Color := clBlack;
    if DockSite.Align in [alLeft, alRight, alTop, alBottom] then
    begin
      Canvas.MoveTo(Left + 9, Top + 10);
      Canvas.LineTo(Left + 9, Top + 7);
      Canvas.MoveTo(Left + 6, Top + 7);
      Canvas.LineTo(Left + 13, Top + 7);
      Canvas.MoveTo(Left + 7, Top + 6);
      Canvas.LineTo(Left + 7, Top + 2);
      Canvas.LineTo(Left + 10, Top + 2);
      Canvas.LineTo(Left + 10, Top + 6);
      Canvas.LineTo(Left + 11, Top + 6);
      Canvas.LineTo(Left + 11, Top + 1);
    end
    else if DockSite.Align in [alNone] then
    begin
      Canvas.MoveTo(Left + 5, Top + 6);
      Canvas.LineTo(Left + 8, Top + 6);
      Canvas.MoveTo(Left + 8, Top + 3);
      Canvas.LineTo(Left + 8, Top + 10);
      Canvas.MoveTo(Left + 9, Top + 4);
      Canvas.LineTo(Left + 12, Top + 4);
      Canvas.LineTo(Left + 12, Top + 7);
      Canvas.LineTo(Left + 9, Top + 7);
      Canvas.LineTo(Left + 9, Top + 8);
      Canvas.LineTo(Left + 13, Top + 8);
    end;
  end;
end;

procedure TJvDockVSNETTree.DrawCloseButton(Canvas: TCanvas;
  Zone: TJvDockZone; Left, Top: Integer);
var
  DrawRect: TRect;
  AZone: TJvDockVSNETZone;
  ColorArr: array[1..2] of TColor;
  ADockClient: TJvDockClient;
  AForm: TForm;
begin
  if Zone <> nil then
  begin

    ADockClient := FindDockClient(Zone.ChildControl);
    if (ADockClient <> nil) and (not ADockClient.EnableCloseButton) then Exit;
    if Zone.ChildControl is TJvDockTabHostForm then
    begin
      AForm := TJvDockTabHostForm(Zone.ChildControl).GetActiveDockForm;
      if AForm <> nil then
      begin
        ADockClient := FindDockClient(AForm);
        if (ADockClient <> nil) and (not ADockClient.EnableCloseButton) then
          Exit;
      end;
    end;
    AZone := TJvDockVSNETZone(Zone);

    DrawRect.Left := Left + 6;
    DrawRect.Right := DrawRect.Left + 7;
    DrawRect.Top := Top + 3;
    DrawRect.Bottom := DrawRect.Top + 7;

    if AZone.CloseButtonState <> bsNormal then
    begin
      if AZone.CloseButtonState = bsUp then
      begin
        ColorArr[1] := clBlack;
        if GetActiveControl = AZone.ChildControl then
          ColorArr[2] := clBtnface
        else
          ColorArr[2] := clWhite;
      end
      else if AZone.CloseButtonState = bsDown then
      begin
        ColorArr[1] := clBtnface;
        ColorArr[2] := clBlack;
      end;
      Canvas.Pen.Color := ColorArr[1];
      Canvas.MoveTo(Left, Top + ButtonHeight);
      Canvas.LineTo(Left + ButtonWidth, Top + ButtonHeight);
      Canvas.LineTo(Left + ButtonWidth, Top);
      Canvas.Pen.Color := ColorArr[2];
      Canvas.LineTo(Left, Top);
      Canvas.LineTo(Left, Top + ButtonHeight);
    end;

    if AZone.CloseButtonState = bsDown then
      OffsetRect(DrawRect, 1, 1);

    if AZone.ChildControl = GetActiveControl then
      Canvas.Pen.Color := clWhite
    else
      Canvas.Pen.Color := clBlack;
    Canvas.MoveTo(DrawRect.Left, DrawRect.Top);
    Canvas.LineTo(DrawRect.Right, DrawRect.Bottom);
    Canvas.MoveTo(DrawRect.Right - 1, DrawRect.Top);
    Canvas.LineTo(DrawRect.Left - 1, DrawRect.Bottom);
  end;
end;

procedure TJvDockVSNETTree.GetCaptionRect(var Rect: TRect);
begin
  if DockSite.Align = alClient then
    inherited
  else
  begin
    Inc(Rect.Left, 2 + CaptionLeftOffset);
    Inc(Rect.Top, 3);
    Dec(Rect.Right, 2 * ButtonWidth + ButtonSplitter + CaptionRightOffset - 1);
    Dec(Rect.Bottom, 2);
  end;
end;

function TJvDockVSNETTree.GetTopGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TJvDockZone): TJvDockZone;
begin
  Result := inherited GetTopGrabbersHTFlag(MousePos, HTFlag, Zone);
  if (Zone <> nil) and (DockSite.Align <> alClient) and (HTFlag <> HTCLOSE) then
  begin
    with Zone.ChildControl do
      if PtInRect(Rect(
        Left + Width - 2 * ButtonWidth - RightOffset - ButtonSplitter,
        Top - GrabberSize + TopOffset,
        Left + Width - ButtonWidth - RightOffset - ButtonSplitter,
        Top - GrabberSize + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTAUTOHIDE;
  end;
end;

procedure TJvDockVSNETTree.DrawDockGrabber(
  Control: TControl; const ARect: TRect);
begin
  inherited;
  if DockSite.Align <> alClient then
    DrawAutoHideButton(FindControlZone(Control), ARect.Right - RightOffset - 2 * ButtonWidth - ButtonSplitter, ARect.Top
      + TopOffset);
end;

procedure TJvDockVSNETTree.PaintDockGrabberRect(Canvas: TCanvas;
  Control: TControl; const ARect: TRect);
var
  DrawRect: TRect;
begin
  inherited;
  if GetActiveControl <> Control then
  begin
    Canvas.Pen.Color := clGray;
    DrawRect := ARect;
    Inc(DrawRect.Left);
    Canvas.RoundRect(DrawRect.Left, DrawRect.Top, DrawRect.Right, DrawRect.Bottom, 2, 2);
  end;
end;

procedure TJvDockVSNETTree.DoLButtonDbClk(var Message: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer);
begin
  if DockSite is TJvDockVSPopupPanel then Exit;
  inherited;
end;

procedure TJvDockVSNETTree.DoHideZoneChild(AZone: TJvDockZone);
var
  AForm: TForm;
  ADockClient: TJvDockClient;
begin

  if (AZone <> nil) and (AZone.ChildControl <> nil) then
  begin
    if AZone.ChildControl is TJvDockTabHostForm then
    begin
      AForm := TJvDockTabHostForm(AZone.ChildControl).GetActiveDockForm;
      if AForm <> nil then
      begin
        ADockClient := FindDockClient(AForm);
        if (ADockClient <> nil) and (not ADockClient.EnableCloseButton) then
          Exit
        else
          AForm.Close;
      end;
    end
    else
      inherited;
  end;
end;

procedure TJvDockVSNETTree.IgnoreZoneInfor(Stream: TMemoryStream);
begin
  inherited;
  Stream.Position := Stream.Position + 1;
end;

constructor TJvDockVSNETConjoinServerOption.Create(
  ADockStyle: TJvDockBasicStyle);
begin
  inherited;
  SystemInfo := True;
end;

destructor TJvDockVSNETConjoinServerOption.Destroy;
begin
  inherited;

end;

procedure TJvDockVSNETConjoinServerOption.SetDefaultSystemCaptionInfo;
begin
  inherited;

  ActiveFont.Color := clWhite;
  ActiveFont.Style := [];

  InactiveFont.Color := clBlack;
  InactiveFont.Style := [];

  SetActiveTitleEndColor_WithoutChangeSystemInfo(ActiveTitleStartColor);
  SetInactiveTitleStartColor_WithoutChangeSystemInfo(clBtnFace);
  SetInactiveTitleEndColor_WithoutChangeSystemInfo(clBtnFace);

end;

constructor TJvDockVSNETTabServerOption.Create(ADockStyle: TJvDockBasicStyle);
begin
  inherited;
  InactiveFont.Color := VSNETPageInactiveFontColor;
  InactiveSheetColor := VSNETPageInactiveSheetColor;
  ShowTabImages := True;
end;

constructor TJvDockVSNETZone.Create(Tree: TJvDockTree);
begin
  inherited;
  FAutoHideButtonState := bsNormal;
  FCloseButtonState := bsNormal;
  FVSPaneVisible := True;
end;

procedure TJvDockVSNETZone.DoCustomSetControlName;
var
  i: Integer;
  Pane: TJvDockVSPane;
  DockClient: TJvDockClient;
begin
  inherited;
  if Tree.DockSite is TJvDockVSPopupPanel then
  begin
    with TJvDockVSPopupPanel(Tree.DockSite).VSChannel do
    begin
      AddDockControl(ChildControl);
      if ChildControl is TJvDockTabHostForm then
      begin
        with TJvDockTabHostForm(ChildControl).PageControl do
        begin
          for i := 0 to DockClientCount - 1 do
          begin
            Pane := FindPane(TWinControl(DockClients[i]));
            DockClient := FindDockClient(DockClients[i]);
            if (Pane <> nil) and (DockClient <> nil) then
            begin
              Pane.Width := DockClient.VSPaneWidth;
            end;
          end;
        end;
      end
      else
      begin
        Pane := FindPane(ChildControl);
        DockClient := FindDockClient(ChildControl);
        if (Pane <> nil) and (DockClient <> nil) then
        begin
          Pane.Width := DockClient.VSPaneWidth;
        end;
      end;
    end;
  end;
end;

procedure TJvDockVSNETZone.SetAutoHideButtonDown(const Value: Boolean);
begin
  FAutoHideButtonDown := Value;
end;

procedure TJvDockVSNETZone.SetAutoHideButtonState(const Value: TJvDockBtnState);
begin
  if FAutoHideButtonState <> Value then
  begin
    FAutoHideButtonState := Value;
    Tree.DockSite.Invalidate;
  end;
end;

procedure TJvDockVSNETZone.SetChildControlVisible(Client: TControl;
  AViisible: Boolean);

begin
  inherited;

end;

procedure TJvDockVSNETZone.SetCloseButtonState(const Value: TJvDockBtnState);
begin
  if FCloseButtonState <> Value then
  begin
    FCloseButtonState := Value;
    Tree.DockSite.Invalidate;
  end;
end;

procedure TJvDockVSNETZone.SetVSPaneVisible(const Value: Boolean);
begin
  FVSPaneVisible := Value;
end;

constructor TJvDockVSNETTabPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabHeight := 25;
  CaptionTopOffset := 1;
end;

constructor TJvDockVSNETTabPageControl.Create(AOwner: TComponent);
begin
  inherited;
  TabSheetClass := TJvDockVSNETTabSheet;
  TabPanelClass := TJvDockVSNETTabPanel;
end;

procedure TJvDockVSNETTabPageControl.CreatePanel;
begin
  inherited;
end;

procedure TJvDockVSNETTabPageControl.ShowControl(AControl: TControl);
begin
  inherited;
end;

procedure TJvDockVSChannel.AddDockControl(Control: TWinControl);
var
  Block: TJvDockVSBlock;
begin
  if Control is TJvDockTabHostForm then
  begin
    Block := TJvDockVSBlock.Create(Self);
    Block.AddDockControl(Control);
    FBlockList.Add(Block);
  end
  else
  begin
    if (Count >= 1) and (Blocks[0].BlockType = btConjoinBlock) then
    begin
      Blocks[0].AddDockControl(Control);
    end
    else
    begin
      Block := TJvDockVSBlock.Create(Self);
      Block.AddDockControl(Control);
      FBlockList.Insert(0, Block);
    end;
  end;
  HideAllPopupPanel(Self);
  ResetPosition;
  Invalidate;
end;

constructor TJvDockVSChannel.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TJvDockVSNETPanel then
  begin
    FVSNETDockPanel := TJvDockVSNETPanel(AOwner);
    DockServer := FVSNETDockPanel.DockServer;
  end;
  FBlockList := TList.Create;
  FChannelWidth := 22;
  FBlockStartOffset := 2;
  FBlockUpOffset := 2;
  FBlockInterval := 13;
  Color := VSNETPageInactiveSheetColor;
  ParentFont := True;
  ActivePaneSize := MaxActivePaneWidth;
end;

procedure TJvDockVSChannel.CreateVSPopupPanel;
begin
  FVSPopupPanel := TJvDockVSPopupPanel.Create(FVSNETDockPanel);
  FVSPopupPanel.Name := FVSNETDockPanel.Name + '_PopupPanel';
  FVSPopupPanel.Visible := False;
  if Parent is TForm then
  begin
    FVSPopupPanel.Parent := Parent;
    FVSPopupPanel.Align := alNone;
    FVSPopupPanel.BringToFront;
  end;
  FVSPopupPanelSplitter := TJvDockVSPopupPanelSplitter.Create(Parent);
  if Parent is TForm then
  begin
    FVSPopupPanelSplitter.Parent := Parent;
    FVSPopupPanelSplitter.Align := alNone;
    FVSPopupPanelSplitter.VSPopupPanel := VSPopupPanel;
    FVSPopupPanelSplitter.Color := clBtnface;
    FVSPopupPanelSplitter.Visible := False;
  end;
end;

procedure TJvDockVSChannel.DeleteBlock(Index: Integer);
begin
  Blocks[Index].Free;
  FBlockList.Delete(Index);
end;

destructor TJvDockVSChannel.Destroy;
begin
  FreeBlockList;
  inherited;
end;

procedure TJvDockVSChannel.DestroyVSPopupPanel;
begin

end;

function TJvDockVSChannel.FindDockControl(Control: TWinControl; var BlockIndex: Integer;
  var PaneIndex: Integer): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  BlockIndex := -1;
  PaneIndex := -1;
  if Control = nil then Exit;
  for i := 0 to Count - 1 do
  begin
    for j := 0 to Blocks[i].Count - 1 do
      if Blocks[i].VSPanes[j].DockForm = Control then
      begin
        BlockIndex := i;
        PaneIndex := j;
        Result := True;
        Exit;
      end;
    if Blocks[i].FBlockType = btTabBlock then
    begin
      j := 0;
      if Blocks[i].VSPanes[0].DockForm.HostDockSite.Parent = Control then
      begin
        BlockIndex := i;
        PaneIndex := j;
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TJvDockVSChannel.GetCount: Integer;
begin
  Result := FBlockList.Count;
end;

procedure TJvDockVSChannel.GetBlockRect(Block: TJvDockVSBlock; Index: Integer;
  var ARect: TRect);
var
  BlockWidth: Integer;
begin
  if Block.VSPanes[Index].DockForm <> Block.FActiveDockControl then
    BlockWidth := Block.InactiveBlockWidth
  else
    BlockWidth := Block.ActiveBlockWidth;

  case Align of
    alLeft:
      begin
        ARect.Left := -1;
        ARect.Top := FCurrentPos;
        ARect.Right := Width - FBlockUpOffset;
        ARect.Bottom := ARect.Top + BlockWidth;
      end;
    alRight:
      begin
        ARect.Left := FBlockUpOffset;
        ARect.Top := FCurrentPos;
        ARect.Right := Width + 1;
        ARect.Bottom := ARect.Top + BlockWidth;
      end;
    alTop:
      begin
        ARect.Left := FCurrentPos;
        ARect.Top := -1;
        ARect.Right := ARect.Left + BlockWidth;
        ARect.Bottom := Height - FBlockUpOffset;
      end;
    alBottom:
      begin
        ARect.Left := FCurrentPos;
        ARect.Top := FBlockUpOffset;
        ARect.Right := ARect.Left + BlockWidth;
        ARect.Bottom := Height + 1;
      end;
  end;

  Inc(FCurrentPos, BlockWidth - 1);
end;

function TJvDockVSChannel.GetBlocks(Index: Integer): TJvDockVSBlock;
begin
  Result := TJvDockVSBlock(FBlockList[Index]);
end;

function TJvDockVSChannel.GetDockFormWithMousePos(MousePos: TPoint): TJvDockVSPane;
var
  i, j: Integer;
  ARect: TRect;
begin
  Result := nil;
  FCurrentPos := FBlockStartOffset;
  for i := 0 to Count - 1 do
  begin
    for j := 0 to Blocks[i].Count - 1 do
    begin
      if not Blocks[i].VSPanes[j].Visible then Continue;
      GetBlockRect(Blocks[i], j, ARect);
      if PtInRect(ARect, MousePos) then
      begin
        Result := Blocks[i].VSPanes[j];
        Exit;
      end;
    end;
    Inc(FCurrentPos, FBlockInterval);
  end;
end;

procedure TJvDockVSChannel.HidePopupPanel(Pane: TJvDockVSPane);
begin
  if Pane <> nil then
  begin
    if Align in [alLeft, alRight] then
    begin
      VSPopupPanel.Width := 0;
      VSPopupPanelSplitter.Width := 0;
    end
    else if Align in [alTop, alBottom] then
    begin
      VSPopupPanel.Height := 0;
      VSPopupPanelSplitter.Height := 0;
    end;
    FActiveDockForm := nil;
    FActivePane := nil;
  end;
  VSPopupPanel.Visible := False;
  VSPopupPanelSplitter.Visible := False;
  FActivePane := nil;
end;

procedure TJvDockVSChannel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  VSPane: TJvDockVSPane;
begin
  inherited;
  VSPane := GetDockFormWithMousePos(Point(X, Y));
  if VSPane <> nil then
  begin
    VSPane.Active := True;
    if VSPane.DockForm.CanFocus then
      VSPane.DockForm.SetFocus;
  end;
end;

procedure TJvDockVSChannel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  PopupDockForm(GetDockFormWithMousePos(Point(X, Y)));
end;

procedure TJvDockVSChannel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

end;

procedure TJvDockVSChannel.Paint;

  procedure DrawSingleBlock(Block: TJvDockVSBlock);
  var
    DrawRect: TRect;
    i: Integer;

    procedure AdjustImagePos;
    begin
      if Align = alLeft then
      begin
        Inc(DrawRect.Left, 3);
        Inc(DrawRect.Top, 4);
      end
      else if Align = alTop then
      begin
        Inc(DrawRect.Left, 4);
        Inc(DrawRect.Top, 2);
      end
      else if Align = alRight then
      begin
        Inc(DrawRect.Left, 4);
        Inc(DrawRect.Top, 4);
      end
      else if Align = alBottom then
      begin
        Inc(DrawRect.Left, 4);
        Inc(DrawRect.Top, 3);
      end;
    end;

  var
    OldGraphicsMode: Integer;
    VisiblePaneCount: Integer;
  begin
    VisiblePaneCount := 0;
    for i := 0 to Block.Count - 1 do
    begin
      if not Block.VSPanes[i].Visible then Continue;

      GetBlockRect(Block, i, DrawRect);

      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(DrawRect);
      Canvas.Brush.Color := clGray;
      Canvas.FrameRect(DrawRect);

      AdjustImagePos;
      Block.FImageList.Draw(Canvas, DrawRect.Left, DrawRect.Top, i);

      if Block.ActiveDockControl = Block.VSPanes[i].DockForm then
      begin
        if Align in [alTop, alBottom] then
          Inc(DrawRect.Left, Block.InactiveBlockWidth)
        else if Align in [alLeft, alRight] then
        begin
          Inc(DrawRect.Top, Block.InactiveBlockWidth);
          if Align = alLeft then
            DrawRect.Left := 15
          else
            DrawRect.Left := 20;
          DrawRect.Right := DrawRect.Left + (DrawRect.Bottom - DrawRect.Top);
        end;
        Canvas.Brush.Color := clBtnFace;
        Canvas.Pen.Color := clBlack;

        Dec(DrawRect.Right, 3);

        OldGraphicsMode := SetGraphicsMode(Canvas.Handle, GM_ADVANCED);
        DrawText(Canvas.Handle, PChar(Block.VSPanes[i].DockForm.Caption), -1, DrawRect, DT_END_ELLIPSIS or DT_NOCLIP);
        SetGraphicsMode(Canvas.Handle, OldGraphicsMode);
      end;
      Inc(VisiblePaneCount);
    end;
    if VisiblePaneCount > 0 then
      Inc(FCurrentPos, FBlockInterval);
  end;

var
  i: Integer;
begin
  inherited;

  FCurrentPos := FBlockStartOffset;
  for i := 0 to Count - 1 do
  begin
    DrawSingleBlock(Blocks[i]);
  end;
end;

procedure TJvDockVSChannel.PopupDockForm(Pane: TJvDockVSPane);
  procedure SetSingleDockFormVisible(HostDockSite: TWinControl; AForm: TForm);
  var
    i: Integer;
  begin
    for i := 0 to HostDockSite.DockClientCount - 1 do
      HostDockSite.DockClients[i].Visible := AForm = HostDockSite.DockClients[i];
  end;

begin
  if (Pane <> nil) and (ActiveDockForm <> Pane.DockForm) then
  begin
    HidePopupPanel(FActivePane);
    Pane.DockForm.Visible := True;
    PopupPanelAnimate.PopupForm(Self, Pane.Width);
    if (Pane.DockForm <> nil) and (Pane.DockForm.HostDockSite.Parent is TJvDockTabHostForm) then
    begin
      FVSPopupPanel.JvDockManager.ShowSingleControl(Pane.DockForm.HostDockSite.Parent);
      SetSingleDockFormVisible(Pane.DockForm.HostDockSite, Pane.DockForm);

      TJvDockTabHostForm(Pane.DockForm.HostDockSite.Parent).Caption := Pane.DockForm.Caption;
    end
    else
      FVSPopupPanel.JvDockManager.ShowSingleControl(Pane.DockForm);
    FActiveDockForm := Pane.DockForm;
    FActivePane := Pane;
    FVSPopupPanel.JvDockManager.ResetBounds(True);

    Pane.Block.FActiveDockControl := Pane.DockForm;
    Invalidate;
  end;
end;

procedure TJvDockVSChannel.RemoveDockControl(Control: TWinControl);
var
  BlockIndex, PaneIndex: Integer;
begin
  VSPopupPanel.Visible := False;
  if FindDockControl(Control, BlockIndex, PaneIndex) then
  begin
    Blocks[BlockIndex].DeletePane(PaneIndex);
    if (Blocks[BlockIndex].Count <= 0) or (Blocks[BlockIndex].FBlockType = btTabBlock) then
      DeleteBlock(BlockIndex);
  end;
  ResetPosition;
  Invalidate;
end;

procedure TJvDockVSChannel.ResetBlock;
var
  i: Integer;
begin
  if Count > 0 then
  begin
    Blocks[0].FBlockStartPos := FBlockStartOffset;
    for i := 1 to Count - 1 do
      Blocks[i].FBlockStartPos := Blocks[i - 1].FBlockStartPos + Blocks[i - 1].GetTotalWidth + FBlockInterval;
  end;
end;

procedure TJvDockVSChannel.ResetPosition;
var
  i, j: Integer;
  PaneCount: Integer;
begin
  PaneCount := 0;
  for i := 0 to Count - 1 do
    for j := 0 to Blocks[i].Count - 1 do
      if Blocks[i].VSPanes[j].Visible = True then
        Inc(PaneCount);

  Visible := PaneCount > 0;
  case Align of
    alLeft:
      begin
        Width := FChannelWidth;
        Left := GetClientAlignControlArea(Parent, Align, Self);
      end;
    alRight:
      begin
        Width := FChannelWidth;
        Left := Parent.ClientWidth - GetClientAlignControlArea(Parent, Align, Self) - FChannelWidth + 1;
      end;
    alTop:
      begin
        Height := FChannelWidth;
        Top := GetClientAlignControlArea(Parent, Align, Self);
      end;
    alBottom:
      begin
        Height := FChannelWidth;
        Top := Parent.ClientHeight - GetClientAlignControlArea(Parent, Align, Self) - FChannelWidth + 1;
      end;
  end;
end;

procedure TJvDockVSChannel.SetVSPopupPanelSplitterPosition;
begin
  case Align of
    alLeft:
      begin
        VSPopupPanelSplitter.Left := VSPopupPanel.Left + VSPopupPanel.Width;
        VSPopupPanelSplitter.Width := VSPopupPanelSplitter.SplitWidth;
        VSPopupPanelSplitter.Top := VSPopupPanel.Top;
        VSPopupPanelSplitter.Height := VSPopupPanel.Height;
      end;
    alRight:
      begin
        VSPopupPanelSplitter.Left := VSPopupPanel.Left - VSPopupPanelSplitter.SplitWidth;
        VSPopupPanelSplitter.Width := VSPopupPanelSplitter.SplitWidth;
        VSPopupPanelSplitter.Top := VSPopupPanel.Top;
        VSPopupPanelSplitter.Height := VSPopupPanel.Height;
      end;
    alTop:
      begin
        VSPopupPanelSplitter.Left := VSPopupPanel.Left;
        VSPopupPanelSplitter.Width := VSPopupPanel.Width;
        VSPopupPanelSplitter.Top := VSPopupPanel.Top + VSPopupPanel.Height;
        VSPopupPanelSplitter.Height := VSPopupPanelSplitter.SplitWidth;
      end;
    alBottom:
      begin
        VSPopupPanelSplitter.Left := VSPopupPanel.Left;
        VSPopupPanelSplitter.Width := VSPopupPanel.Width;
        VSPopupPanelSplitter.Top := VSPopupPanel.Top - VSPopupPanelSplitter.SplitWidth;
        VSPopupPanelSplitter.Height := VSPopupPanelSplitter.SplitWidth;
      end;
  end;
  VSPopupPanelSplitter.Visible := True;
  VSPopupPanelSplitter.BringToFront;
end;

procedure TJvDockVSChannel.SetVSPopupPanelSplitter(
  const Value: TJvDockVSPopupPanelSplitter);
begin
  FVSPopupPanelSplitter := Value;
end;

function TJvDockVSChannel.GetPaneWithControl(AControl: TControl): TJvDockVSPane;
var
  i, j: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    for j := 0 to Blocks[i].Count - 1 do
      if AControl = Blocks[i].VSPanes[j].DockForm then
      begin
        Result := Blocks[i].VSPanes[j];
        Exit;
      end;
end;

procedure TJvDockVSChannel.SetBlockStartOffset(const Value: Integer);
begin
  FBlockStartOffset := Value;
end;

procedure TJvDockVSChannel.AnimatePopupPanel(
  AnimateStyle: TJvDockPoppupPanelAnimateStyle);
begin
  if AnimateStyle = pasShow then
  begin

  end
  else if AnimateStyle = pasHide then
  begin

  end;
end;

procedure TJvDockVSChannel.ResetFontAngle;
var
  LogFont: TLogFont;
begin
  if Align in [alLeft, alRight] then
  begin
    if GetObject(Canvas.Font.Handle, SizeOf(LogFont), @LogFont) <> 0 then
    begin
      LogFont.lfEscapement := 2700;
      LogFont.lfOrientation := 2700;
      Canvas.Font.Handle := CreateFontIndirect(LogFont);
    end;
  end;
end;

procedure TJvDockVSChannel.RemoveAllBlock;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    DeleteBlock(i);
  FActiveDockForm := nil;
end;

procedure TJvDockVSChannel.HidePopupPanel(Control: TWinControl);
var
  BlockIndex, PaneIndex: Integer;
begin
  FindDockControl(Control, BlockIndex, PaneIndex);
  if (BlockIndex >= 0) and (PaneIndex >= 0) then
    HidePopupPanel(Blocks[BlockIndex].VSPanes[PaneIndex]);
end;

procedure TJvDockVSChannel.PopupDockForm(Control: TWinControl);
var
  BlockIndex, PaneIndex: Integer;
begin
  FindDockControl(Control, BlockIndex, PaneIndex);
  if (BlockIndex >= 0) and (PaneIndex >= 0) then
    PopupDockForm(Blocks[BlockIndex].VSPanes[PaneIndex]);
end;

function TJvDockVSChannel.FindPane(Control: TWinControl): TJvDockVSPane;
var
  i, j: Integer;
begin
  Result := nil;
  if FindDockControl(Control, i, j) then
    Result := Blocks[i].VSPanes[j];
end;

procedure TJvDockVSChannel.HidePopupPanelWithAnimate;
begin
  if FActivePane <> nil then
    PopupPanelAnimate.HideForm(Self, FActivePane.Width);
end;

procedure TJvDockVSChannel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

procedure TJvDockVSChannel.ResetActivePaneWidth;
var
  DockClient: TJvDockClient;
begin
  if FActivePane = nil then Exit;
  DockClient := FindDockClient(FActivePane.DockForm);
  if Align in [alLeft, alRight] then
  begin
    FActivePane.Width := VSPopupPanel.Width;
  end
  else if Align in [alTop, alBottom] then
  begin
    FActivePane.Width := VSPopupPanel.Height + VSPopupPanel.JvDockManager.GrabberSize;
  end;
  if DockClient <> nil then
    DockClient.VSPaneWidth := FActivePane.Width;
end;

procedure TJvDockVSChannel.ResetPopupPanelHeight;
begin
  if Align in [alLeft, alRight] then
  begin
    VSPopupPanel.Top := Top;
    VSPopupPanel.Height := Height;
    VSPopupPanelSplitter.Top := Top;
    VSPopupPanelSplitter.Height := Height;
  end;
end;

procedure TJvDockVSChannel.FreeBlockList;
var
  i: Integer;
begin
  for i := 0 to FBlockList.Count - 1 do
    Blocks[i].Free;
  FBlockList.Free;
end;

procedure TJvDockVSChannel.SetActivePaneSize(const Value: Integer);
begin
  if FActivePaneSize <> Value then
  begin
    FActivePaneSize := Value;
    Invalidate;
  end;
end;

procedure TJvDockVSBlock.AddDockControl(Control: TWinControl);

  function GetPaneWidth: Integer;
  begin
    Result := 100;
    if Control = nil then Exit;
    case VSChannel.Align of
      alLeft, alRight:
        Result := Control.Width;
      alTop, alBottom:
        Result := Control.Height;
    end;
  end;

var
  i, PaneWidth: Integer;
  Icon: TIcon;
  DockClient: TJvDockClient;
begin
  PaneWidth := GetPaneWidth;
  if Control is TJvDockTabHostForm then
  begin
    FBlockType := btTabBlock;
    with TJvDockTabHostForm(Control) do
    begin
      for i := 0 to DockableControl.DockClientCount - 1 do
      begin
        FVSPaneList.Add(TJvDockVSPane.Create(Self, TForm(PageControl.DockClients[i]), PaneWidth, FVSPaneList.Count));
        if not JvGlobalDockIsLoading then
        begin
          DockClient := FindDockClient(PageControl.DockClients[i]);
          if DockClient <> nil then
            DockClient.VSPaneWidth := PaneWidth;
        end;
        if TForm(PageControl.DockClients[i]).Icon = nil then
        begin
          Icon := TIcon.Create;
          Icon.Width := 16;
          Icon.Height := 16;
          FImageList.AddIcon(Icon);
          Icon.Free;
        end
        else
          FImageList.AddIcon(TForm(PageControl.DockClients[i]).Icon);
        TJvDockVSNETTabSheet(PageControl.Pages[i]).PreviousVisible := PageControl.DockClients[i].Visible;
        if PageControl.Pages[i] <> PageControl.ActivePage then
          PageControl.DockClients[i].Visible := False;
      end;
      for i := 0 to Count - 1 do
      begin
        if VSPanes[i].Visible then
        begin
          FActiveDockControl := VSPanes[i].DockForm;
          Break;
        end;
      end;
    end;
  end
  else
  begin
    FBlockType := btConjoinBlock;
    FVSPaneList.Add(TJvDockVSPane.Create(Self, TForm(Control), PaneWidth, FVSPaneList.Count));
    if not JvGlobalDockIsLoading then
    begin
      DockClient := FindDockClient(Control);
      if DockClient <> nil then
        DockClient.VSPaneWidth := PaneWidth;
    end;
    if TForm(Control).Icon = nil then
    begin
      Icon := TIcon.Create;
      Icon.Width := 16;
      Icon.Height := 16;
      FImageList.AddIcon(Icon);
      Icon.Free;
    end
    else
      FImageList.AddIcon(TForm(Control).Icon);
    FActiveDockControl := Control;
  end;
  ResetActiveBlockWidth;
end;

constructor TJvDockVSBlock.Create(Owner: TJvDockVSChannel);
begin
  FVSChannel := Owner;
  FVSPaneList := TList.Create;
  FImageList := TImageList.CreateSize(16, 16);
  FInactiveBlockWidth := 24;
  FActiveBlockWidth := 24;
end;

destructor TJvDockVSBlock.Destroy;
var
  i: Integer;
begin
  FImageList.Free;
  for i := 0 to Count - 1 do
    VSPanes[i].Free;
  FVSPaneList.Free;
  inherited;
end;

function TJvDockVSBlock.GetVSPane(Index: Integer): TJvDockVSPane;
begin
  Result := TJvDockVSPane(FVSPaneList[Index]);
end;

function TJvDockVSBlock.GetCount: Integer;
begin
  Result := FVSPaneList.Count;
end;

function TJvDockVSBlock.GetTotalWidth: Integer;
begin
  Result := (FVSPaneList.Count - 1) * FInactiveBlockWidth + FActiveBlockWidth;
end;

procedure TJvDockVSBlock.RemoveDockControl(Control: TWinControl);
begin
  ResetActiveBlockWidth;
end;

procedure TJvDockVSBlock.ResetActiveBlockWidth;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    FActiveBlockWidth := Max(FActiveBlockWidth, min(VSChannel.ActivePaneSize,
      TForm(VSChannel.Parent).Canvas.TextWidth(VSPanes[i].DockForm.Caption) + InactiveBlockWidth + 10));
  end;
end;

procedure TJvDockVSBlock.DeletePane(Index: Integer);
var
  i: Integer;
begin
  for i := Index to FVSPaneList.Count - 2 do
    VSPanes[i + 1].Index := VSPanes[i].Index;
  VSPanes[Index].Free;
  FVSPaneList.Delete(Index);
end;

constructor TJvDockVSNETPanel.Create(AOwner: TComponent);
begin
  inherited;
  FVSChannelClass := TJvDockVSChannel;
end;

procedure TJvDockVSNETPanel.CreateVSChannel;
begin
  if (FVSChannelClass <> nil) and
    (FVSChannelClass <> TJvDockVSChannelClass(ClassType)) then
  begin
    FVSChannel := FVSChannelClass.Create(Self);
    FVSChannel.Parent := Parent;
    FVSChannel.Align := Align;

    FVSChannel.ResetFontAngle;

    FVSChannel.ResetPosition;

    FVSChannel.Visible := False;

    FVSChannel.Name := Name + '_VSChannel';

    FVSChannel.CreateVSPopupPanel;
  end;
end;

procedure TJvDockVSNETPanel.CustomDockDrop(Source: TJvDockDragDockObject; X,
  Y: Integer);
begin
  inherited;
  VSChannel.ActiveDockForm.Perform(CM_EXIT, 0, 0);
end;

destructor TJvDockVSNETPanel.Destroy;
begin
  inherited;

end;

procedure TJvDockVSNETPanel.DestroyVSChannel;
begin
end;

procedure TJvDockVSNETPanel.DoAutoHideControl(Control: TWinControl);

begin

  if Align = alNone then
    DoShowControl(Control)
  else
    DoHideControl(Control);
end;

procedure TJvDockVSNETPanel.DoHideControl(Control: TWinControl);
begin
  VSChannel.AddDockControl(Control);
  ShowDockPanel(VisibleDockClientCount > 1, Control, sdfDockPanel);
  Control.Dock(VSChannel.VSPopupPanel, Rect(0, 0, 0, 0));
  VSChannel.VSPopupPanel.JvDockManager.InsertControl(Control, alNone, nil);
  VSChannel.VSPopupPanel.JvDockManager.ShowSingleControl(Control);
  JvDockManager.HideControl(Control);
  ResetChannelBlockStartOffset(VSChannel);
end;

procedure TJvDockVSNETPanel.DoShowControl(Control: TWinControl);
var
  Panel: TJvDockVSNETPanel;

  procedure ResetDockFormVisible;
  var
    i: Integer;
  begin
    if Control is TJvDockTabHostForm then
    begin
      with TJvDockTabHostForm(Control) do
        for i := 0 to PageControl.Count - 1 do
        begin
          PageControl.Pages[i].Visible := TJvDockVSNETTabSheet(PageControl.Pages[i]).PreviousVisible;
          PageControl.Pages[i].Controls[0].Visible := PageControl.Pages[i].Visible;
        end;
    end;
  end;
begin
  if self is TJvDockVSPopupPanel then
  begin
    Panel := TJvDockVSPopupPanel(self).FVSNETDockPanel;
    Control.Dock(Panel, Rect(0, 0, 0, 0));
    Panel.JvDockManager.ShowControl(Control);
    JvDockManager.RemoveControl(Control);
    Panel.VSChannel.RemoveDockControl(Control);
    Panel.ShowDockPanel(Panel.VisibleDockClientCount > 0, Control, sdfDockPanel);
    if (Panel.VSChannel.ActiveDockForm <> nil) and Panel.VSChannel.ActiveDockForm.CanFocus then
      Panel.VSChannel.ActiveDockForm.SetFocus;
    Panel.VSChannel.HidePopupPanel(Panel.VSChannel.FActivePane);
    ResetDockFormVisible;
    ResetChannelBlockStartOffset(Panel.VSChannel);
  end;
end;

procedure TJvDockVSNETPanel.Resize;
begin
  inherited;
  if Align in [alTop, alBottom] then
  begin
    TJvDockVSNETPanel(DockServer.DockPanelWithAlign[alleft]).VSChannel.ResetPopupPanelHeight;
    TJvDockVSNETPanel(DockServer.DockPanelWithAlign[alRight]).VSChannel.ResetPopupPanelHeight;
  end;
end;

procedure TJvDockVSNETPanel.SetDockServer(const Value: TJvDockServer);
begin
  inherited;
  if not (Self is TJvDockVSPopupPanel) then
    CreateVSChannel;

end;

constructor TJvDockVSPane.Create(ABlock: TJvDockVSBlock; AForm: TForm; AWidth: Integer; AIndex: Integer);
begin
  Block := ABlock;
  DockForm := AForm;
  Width := AWidth;
  Active := False;
  Index := AIndex;
  Visible := AForm.Visible;
end;

destructor TJvDockVSPane.Destroy;
begin
  inherited;

end;

constructor TJvDockVSPopupPanel.Create(AOwner: TComponent);
begin
  inherited;
  DockSite := True;
  if AOwner is TJvDockVSNETPanel then
  begin
    FVSNETDockPanel := TJvDockVSNETPanel(AOwner);
    FVSChannel := FVSNETDockPanel.VSChannel;
    DockServer := FVSNETDockPanel.DockServer;
  end;
  Anchors := [akLeft, akRight, akTop, akBottom];
  BoundsRect := Rect(0, 0, 0, 0);
end;

function TJvDockVSPopupPanel.CreateDockManager: IDockManager;
begin
  if (DockManager = nil) and DockSite and UseDockManager then
    Result := TJvDockVSNETTree.Create(
      Self, TJvDockVSNETZone) as IJvDockManager
  else
    Result := DockManager;
end;

destructor TJvDockVSPopupPanel.Destroy;
begin
  inherited;

end;

function TJvDockVSPopupPanel.GetVSChannel: TJvDockVSChannel;
begin
  if FVSNETDockPanel <> nil then
    Result := FVSNETDockPanel.VSChannel
  else
    Result := nil;
end;

procedure TJvDockVSPopupPanel.SetParent(AParent: TWinControl);
begin
  inherited;
  if AParent = nil then Exit;
end;

procedure TJvDockVSPopupPanel.SetVSNETDockPanel(
  const Value: TJvDockVSNETPanel);
begin
  FVSNETDockPanel := Value;
end;

procedure TJvDockVSPopupPanel.ShowDockPanel(MakeVisible: Boolean;
  Client: TControl; PanelSizeFrom: TJvDockSetDockPanelSizeFrom);
begin
  if Align <> alNone then
    inherited;
end;

constructor TJvDockVSNETTabSheet.Create(AOwner: TComponent);
begin
  inherited;
  FPreviousVisible := True;
end;

procedure TJvDockVSNETTabSheet.SetPreviousVisible(const Value: Boolean);
begin
  FPreviousVisible := Value;
end;

type
  TWinControlAccess = class(TWinControl);

constructor TJvDockVSPopupPanelSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSnap := False;
  Align := alNone;
  Height := 0;
  Width := 0;
  FMinSize := 30;
  FResizeStyle := rsPattern;
  FOldSize := -1;
  FSplitWidth := 4;
  Anchors := [akLeft, akRight, akTop, akBottom];
end;

destructor TJvDockVSPopupPanelSplitter.Destroy;
begin
  FBrush.Free;
  inherited Destroy;
end;

procedure TJvDockVSPopupPanelSplitter.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  if ResizeStyle = rsPattern then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;
      FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
    end;
    FPrevBrush := SelectObject(FLineDC, FBrush.Handle);
  end;
end;

procedure TJvDockVSPopupPanelSplitter.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P := Point(Left, Top);
  if VSChannelAlign in [alLeft, alRight] then
    P.X := Left + FSplit
  else
    P.Y := Top + FSplit;
  with P do
    PatBlt(FLineDC, X, Y, Width, Height, PATINVERT);
end;

procedure TJvDockVSPopupPanelSplitter.ReleaseLineDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
end;

function TJvDockVSPopupPanelSplitter.FindControl: TControl;
begin
  Result := FVSPopupPanel;
end;

procedure TJvDockVSPopupPanelSplitter.RequestAlign;
begin
  inherited RequestAlign;

  if VSChannelAlign in [alBottom, alTop] then
    Cursor := crVSplit
  else
    Cursor := crHSplit;
end;

procedure TJvDockVSPopupPanelSplitter.Paint;
const
  XorColor = $00FFD8CE;
var
  FrameBrush: HBRUSH;
  R: TRect;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  InflateRect(R, 2, 2);
  case VSChannelAlign of
    alLeft:
      begin
        Dec(R.Right, 2);
      end;
    alRight:
      begin
        Inc(R.Left, 3);
      end;
    alTop:
      begin
        Dec(R.Bottom, 2);
      end;
    alBottom:
      begin
        Inc(R.Top, 3);
      end;
  end;
  DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_ADJUSTRECT);
  R := ClientRect;
  if Beveled then
  begin
    if VSChannelAlign in [alLeft, alRight] then
      InflateRect(R, -1, 2)
    else
      InflateRect(R, 2, -1);
    OffsetRect(R, 1, 1);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
    OffsetRect(R, -2, -2);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnShadow));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
  end;
  if csDesigning in ComponentState then

    with Canvas do
    begin
      Pen.Style := psDot;
      Pen.Mode := pmXor;
      Pen.Color := XorColor;
      Brush.Style := bsClear;
      Rectangle(0, 0, ClientWidth, ClientHeight);
    end;
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

function TJvDockVSPopupPanelSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  Result := CanResize(NewSize);
  if Result and (NewSize <= MinSize) and FAutoSnap then
    NewSize := 0;
end;

function TJvDockVSPopupPanelSplitter.CanResize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanResize) then FOnCanResize(Self, NewSize, Result);
end;

procedure TJvDockVSPopupPanelSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FControl := FindControl;
    FDownPos := Point(X, Y);
    if Assigned(FControl) then
    begin
      if VSChannelAlign in [alLeft, alRight] then
      begin
        FMaxSize := Parent.ClientWidth - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alLeft, alRight] then Dec(FMaxSize, Width);
        Inc(FMaxSize, FControl.Width);
      end
      else
      begin
        FMaxSize := Parent.ClientHeight - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alTop, alBottom] then Dec(FMaxSize, Height);
        Inc(FMaxSize, FControl.Height);
      end;
      UpdateSize(X, Y);
      AllocateLineDC;
      with ValidParentForm(Self) do
        if ActiveControl <> nil then
        begin
          FActiveControl := ActiveControl;
          FOldKeyDown := TWinControlAccess(FActiveControl).OnKeyDown;
          TWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
        end;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TJvDockVSPopupPanelSplitter.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    case VSChannelAlign of
      alLeft:
        begin
          FControl.Width := FNewSize;
          Left := FControl.Left + FNewSize;
        end;
      alTop:
        begin
          FControl.Height := FNewSize;
          Top := FControl.Top + FNewSize;
        end;
      alRight:
        begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - FNewSize);
            FControl.Width := FNewSize;
            Left := FControl.Left - Width;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          Parent.DisableAlign;
          try
            FControl.Top := FControl.Top + (FControl.Height - FNewSize);
            FControl.Height := FNewSize;
            Top := FControl.Top - Height;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    FVSPopupPanel.VSChannel.ResetActivePaneWidth;
    Update;
    if Assigned(FOnMoved) then FOnMoved(Self);
    FOldSize := FNewSize;
  end;
end;

procedure TJvDockVSPopupPanelSplitter.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  if VSChannelAlign in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case VSChannelAlign of
    alLeft: S := FControl.Width + Split;
    alRight: S := FControl.Width - Split;
    alTop: S := FControl.Height + Split;
    alBottom: S := FControl.Height - Split;
  end;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize
  else if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    if VSChannelAlign in [alRight, alBottom] then
      S := S - NewSize
    else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;

procedure TJvDockVSPopupPanelSplitter.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

procedure TJvDockVSPopupPanelSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewSize, Split: Integer;
begin
  inherited;
  if (ssLeft in Shift) and Assigned(FControl) then
  begin
    CalcSplitSize(X, Y, NewSize, Split);
    if DoCanResize(NewSize) then
    begin
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
      FNewSize := NewSize;
      FSplit := Split;
      if ResizeStyle = rsUpdate then UpdateControlSize;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TJvDockVSPopupPanelSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Assigned(FControl) then
  begin
    if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    UpdateControlSize;
    StopSizing;
  end;
end;

procedure TJvDockVSPopupPanelSplitter.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
  else if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

procedure TJvDockVSPopupPanelSplitter.SetBeveled(Value: Boolean);
begin
  FBeveled := Value;
  Repaint;
end;

procedure TJvDockVSPopupPanelSplitter.StopSizing;
begin
  if Assigned(FControl) then
  begin
    if FLineVisible then DrawLine;
    FControl := nil;
    ReleaseLineDC;
    if Assigned(FActiveControl) then
    begin
      TWinControlAccess(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

procedure TJvDockVSPopupPanelSplitter.SetVSPopupPanel(
  const Value: TJvDockVSPopupPanel);
begin
  Assert((Value <> nil) and (Value is TJvDockVSPopupPanel));
  FVSPopupPanel := Value;
end;

function TJvDockVSPopupPanelSplitter.GetVSChannelAlign: TAlign;
begin
  Result := alNone;
  if (VSPopupPanel <> nil) and (VSPopupPanel.FVSNETDockPanel <> nil) then
    Result := VSPopupPanel.FVSNETDockPanel.Align;
end;

procedure TJvDockVSPopupPanelSplitter.SetSplitWidth(const Value: Integer);
begin
  FSplitWidth := Value;
end;

constructor TPopupPanelAnimate.Create(AOwner: TComponent);
begin
  inherited;
  Interval := TJvDockVSNetStyle.GetAnimationInterval;
  Enabled := False;
  FMaxWidth := 0;
  FCurrentWidth := 0;
  OnTimer := OnCustomTimer;
  FState := asPopup;
end;

destructor TPopupPanelAnimate.Destroy;
begin
  inherited;

end;

procedure TPopupPanelAnimate.HideForm(VSChannel: TJvDockVSChannel; MaxWidth: Integer);
begin
  if FVSChannel <> nil then Exit;
  FVSChannel := VSChannel;
  Enabled := (FVSChannel <> nil) and (FVSChannel.ActiveDockForm <> nil);
  if FVSChannel <> nil then
  begin
    FMaxWidth := MaxWidth;
    FCurrentWidth := 0;
    FState := asHide;
  end;
end;

procedure TPopupPanelAnimate.OnCustomTimer(Sender: TObject);
begin
  // ??? no handler?
end;

procedure TPopupPanelAnimate.PopupForm(VSChannel: TJvDockVSChannel; MaxWidth: Integer);
begin
  if (FCurrentWidth > 0) and (FVSChannel <> nil) then
  begin
    FVSChannel.Parent.EnableAlign;
  end;
  FVSChannel := VSChannel;
  Enabled := FVSChannel <> nil;
  if FVSChannel <> nil then
  begin
    FMaxWidth := MaxWidth;
    FCurrentWidth := 0;
    FState := asPopup;
  end;
end;

procedure TPopupPanelAnimate.Timer;
  procedure SetControlBringToFront(Control: TWincontrol; Align: TAlign);
  var
    i: Integer;
  begin
    for i := Control.ControlCount - 1 downto 0 do
    begin
      if Control.Controls[i].Visible and (Control.Controls[i].Align = Align)
        and not (Control.Controls[i] is TJvDockVSChannel) and not (Control.Controls[i] is TJvDockPanel)
        and not (Control.Controls[i] is TJvDockSplitter) then
        Control.Controls[i].BringToFront;
    end;
  end;
var
  SuitablyWidth: Integer;
begin
  inherited;
  if FVSChannel <> nil then
  begin
    SuitablyWidth := min(FCurrentWidth, FMaxwidth);
    with FVSChannel do
    begin
      if FCurrentWidth = 0 then
      begin
        Parent.DisableAlign;
        VSPopupPanel.BringToFront;
        VSPopupPanelSplitter.BringToFront;
        SetControlBringToFront(Parent, Align);
        BringToFront;
      end;
      case Align of
        alLeft:
          begin
            if FState = asPopup then
            begin
              if FCurrentWidth = 0 then
              begin
                VSPopupPanel.Width := FMaxWidth;
                VSPopupPanel.Top := Top;
                VSPopupPanel.Height := Height;
                VSPopupPanelSplitter.Top := Top;
                VSPopupPanelSplitter.Height := Height;
                VSPopupPanelSplitter.Width := VSPopupPanelSplitter.SplitWidth;
              end;
              VSPopupPanel.Left := Left + Width + SuitablyWidth - VSPopupPanel.Width;
            end
            else if FState = asHide then
              VSPopupPanel.Left := Left - FCurrentWidth;

            VSPopupPanelSplitter.Left := VSPopupPanel.Left + VSPopupPanel.Width;
          end;

        alRight:
          begin
            if FState = asPopup then
            begin
              if FCurrentWidth = 0 then
              begin
                VSPopupPanel.Width := FMaxWidth;
                VSPopupPanel.Top := Top;
                VSPopupPanel.Height := Height;
                VSPopupPanelSplitter.Top := Top;
                VSPopupPanelSplitter.Height := Height;
                VSPopupPanelSplitter.Width := VSPopupPanelSplitter.SplitWidth;
              end;
              VSPopupPanel.Left := Left - SuitablyWidth;
            end
            else if FState = asHide then
              VSPopupPanel.Left := Left - VSPopupPanel.Width + FCurrentWidth;

            VSPopupPanelSplitter.Left := VSPopupPanel.Left - VSPopupPanelSplitter.SplitWidth;
          end;

        alTop:
          begin
            if FState = asPopup then
            begin
              if FCurrentWidth = 0 then
              begin
                VSPopupPanel.Left := Left;
                VSPopupPanel.Height := FMaxWidth;
                VSPopupPanel.Width := Width;
                VSPopupPanelSplitter.Left := Left;
                VSPopupPanelSplitter.Width := Width;
                VSPopupPanelSplitter.Height := VSPopupPanelSplitter.SplitWidth;
              end;
              VSPopupPanel.Top := Top + Height + SuitablyWidth - VSPopupPanel.Height;
            end
            else if FState = asHide then
              VSPopupPanel.Top := Top - FCurrentWidth;

            VSPopupPanelSplitter.Top := VSPopupPanel.Top + VSPopupPanel.Height;
          end;

        alBottom:
          begin
            if FState = asPopup then
            begin
              if FCurrentWidth = 0 then
              begin
                VSPopupPanel.Left := Left;
                VSPopupPanel.Width := Width;
                VSPopupPanel.Height := FMaxWidth;
                VSPopupPanelSplitter.Left := Left;
                VSPopupPanelSplitter.Width := Width;
                VSPopupPanelSplitter.Height := VSPopupPanelSplitter.SplitWidth;
              end;
              VSPopupPanel.Top := Top - SuitablyWidth;
            end
            else if FState = asHide then
              VSPopupPanel.Top := Top - VSPopupPanel.Height + FCurrentWidth;

            VSPopupPanelSplitter.Top := VSPopupPanel.Top - VSPopupPanelSplitter.SplitWidth;
          end;
      end;
      VSPopupPanel.Visible := True;
      VSPopupPanelSplitter.Visible := True;
    end;
    if FCurrentWidth >= FMaxwidth then
    begin
      FVSChannel.Parent.EnableAlign;
      Enabled := False;
      if FState = asHide then
        FVSChannel.HidePopupPanel(FVSChannel.FActivePane)
      else
      begin
        if FVSChannel.ActiveDockForm <> nil then
        begin
          if FVSChannel.ActiveDockForm.CanFocus then
            FVSChannel.ActiveDockForm.SetFocus;
        end;
      end;
      FVSChannel := nil;
      FCurrentWidth := 0;
      FMaxwidth := 0;
    end
    else
      Inc(FCurrentWidth, TJvDockVSNETStyle.GetAnimationMoveWidth);
  end;
end;

constructor TJvDockVSNETChannelOption.Create(ADockStyle: TJvDockBasicStyle);
begin
  inherited;
  FActivePaneSize := 100;
  FShowImage := True;
  FMouseleaveHide := False;
  FHideHoldTime := 1000;
end;

procedure TJvDockVSNETChannelOption.ResetDockClientOption(
  ADockClient: TJvDockClient);
var
  VSChannel: TJvDockVSChannel;

  procedure ResetActiveBlockSize;
  begin
    if VSChannel <> nil then
      VSChannel.ActivePaneSize := ActivePaneSize;
  end;

begin
  if ADockClient = nil then Exit;
  if ADockClient.ParentForm.HostDockSite is TJvDockVSPopupPanel then
    VSChannel := TJvDockVSPopupPanel(ADockClient.ParentForm.HostDockSite).VSChannel
  else if (ADockClient.ParentForm.HostDockSite is TJvDockVSNETTabPageControl) and
    (ADockClient.ParentForm.HostDockSite.Parent.HostDockSite is TJvDockVSPopupPanel) then
    VSChannel := TJvDockVSPopupPanel(ADockClient.ParentForm.HostDockSite.Parent.HostDockSite).VSChannel;
  ResetActiveBlockSize;
end;

procedure TJvDockVSNETChannelOption.ResetDockControlOption;
var
  i: Integer;
  ADockServer: TJvDockServer;
begin
  if DockStyle = nil then Exit;

  for i := 0 to DockStyle.Count - 1 do
  begin
    if DockStyle.DockBaseControlLists[i] is TJvDockServer then
    begin

      ADockServer := TJvDockServer(DockStyle.DockBaseControlLists[i]);
      ResetDockServerOption(ADockServer);
    end;
  end;
end;

procedure TJvDockVSNETChannelOption.ResetDockServerOption(
  ADockServer: TJvDockServer);
var
  VSChannel: TJvDockVSChannel;

  procedure ResetActiveBlockSize;
  begin
    if VSChannel <> nil then
      VSChannel.ActivePaneSize := ActivePaneSize;
  end;

var
  i: Integer;
begin
  if ADockServer = nil then Exit;
  for i := 0 to 3 do
  begin
    if ADockServer.DockPanel[i] = nil then Continue;
    VSChannel := TJvDockVSNETPanel(ADockServer.DockPanel[i]).VSChannel;
    ResetActiveBlockSize;
  end;
end;

procedure TJvDockVSNETChannelOption.SetActivePaneSize(const Value: Integer);
begin
  if FActivePaneSize <> Value then
  begin
    FActivePaneSize := Max(24, Value);
    ResetDockControlOption;
  end;
end;

procedure TJvDockVSNETChannelOption.SetHideHoldTime(const Value: Integer);
begin
  if FHideHoldTime <> Value then
  begin
    if Value < 100 then
    begin
      if (csDesigning in DockStyle.ComponentState) then
        ShowMessage('HideHoldTime cannot be less than 100');
      FHideHoldTime := 100;
    end
    else
      FHideHoldTime := Value;
  end;
end;

procedure TJvDockVSNETChannelOption.SetMouseleaveHide(const Value: Boolean);
begin
  if FMouseleaveHide <> Value then
    FMouseleaveHide := Value;
end;

procedure TJvDockVSNETChannelOption.SetShowImage(const Value: Boolean);
begin
  FShowImage := Value;
end;

constructor TJvDockAppEvents.Create(AOwner: TComponent);
begin
  inherited;
  FOldOnMessage := OnMessage;
  OnMessage := NewOnMessage;
end;

procedure TJvDockAppEvents.NewOnMessage(var Msg: TMsg; var Handled: Boolean);
var
  CurrControl: TWinControl;
  DockServer: TJvDockServer;
  VSChannel: TJvDockVSChannel;
  i, j: Integer;

  function CanHide: Boolean;
  begin
    Result := False;
    CurrControl := FindControl(Msg.hwnd);
    if CurrControl = nil then Exit;
    repeat

      if csDesigning in CurrControl.ComponentState then
      begin
        Result := False;
        Exit;
      end;

      Result := not ((CurrControl is TJvDockVSChannel) or (CurrControl is TJvDockVSPopupPanel) or (CurrControl is
        TJvDockVSPopupPanelSplitter));
      CurrControl := CurrControl.Parent;
    until (CurrControl = nil) or not Result;

  end;
begin
  if Assigned(FOldOnMessage) then
    FOldOnMessage(Msg, Handled);
  if (Msg.message = WM_LBUTTONDOWN) then
  begin
    if CanHide then
    begin
      for i := 0 to Screen.CustomFormCount - 1 do
      begin
        DockServer := FindDockServer(Screen.CustomForms[i]);
        if (DockServer <> nil) and (DockServer.DockStyle is TJvDockVSNetStyle) then
        begin
          if DockServer.DockPanel[0] = nil then Exit;
          for j := 0 to 3 do
          begin
            VSChannel := TJvDockVSNETPanel(DockServer.DockPanel[j]).VSChannel;
            VSChannel.HidePopupPanelWithAnimate;
          end;
        end;
      end;

    end;
  end;
end;

initialization

finalization
  PopupPanelAnimate.Free;
  ApplicationEvents.Free;

end.

