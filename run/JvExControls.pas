{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExControls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2004-01-13

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I jvcl.inc}

unit JvExControls;
interface
uses
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, QGraphics, QControls, QWindows, QForms,  // order: QControls, QWindows
  {$ENDIF VisualCLX}
  Classes, SysUtils,
  JvThemes;

{$IFDEF VCL}
 {$DEFINE NeedMouseEnterLeave}
{$ELSE}
 {$IF not declared(PatchedVCLX)}
  {$DEFINE NeedMouseEnterLeave}
 {$IFEND}
{$ENDIF VCL}

type
  TDlgCode = (
    dcWantAllKeys, dcWantArrows, dcWantTab, dcWantChars,
    dcButton,
    dcNative // if dcNative is in the set the native functions are used and DoGetDlgCode is ignored
  );
  TDlgCodes = set of TDlgCode;
const
  dcWantMessage = dcWantAllKeys;

type
  IJvControlEvents = interface
    ['{61FC57FF-D4DA-4840-B871-63DE804E9921}']
    procedure VisibleChanged;
    procedure EnabledChanged;
    procedure TextChanged;
    procedure FontChanged;
    procedure ColorChanged;
    procedure ParentFontChanged;
    procedure ParentColorChanged;
    procedure ParentShowHintChanged;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean;
    function HintShow(var HintInfo: THintInfo): Boolean;
    function HitTest(X, Y: Integer): Boolean;
    procedure MouseEnter(AControl: TControl);
    procedure MouseLeave(AControl: TControl);
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
    {$IFDEF VCL}
    procedure SetAutoSize(Value: Boolean);
    {$ENDIF VCL}
  end;

  IJvWinControlEvents = interface
    ['{B5F7FB62-78F0-481D-AFF4-7A24ED6776A0}']
    procedure CursorChanged;
    procedure ShowingChanged;
    procedure ShowHintChanged;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean);
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean);
    procedure DoGetDlgCode(var Code: TDlgCodes);
  end;

  IJvCustomControlEvents = interface
    ['{7804BD3A-D7A5-4314-9259-6DE08A0DC38A}']
  end;

const
  CM_DENYSUBCLASSING = CM_BASE + 2000; // from JvThemes.pas

type
  { Add IJvDenySubClassing to the base class list if the control should not
    be themed by the ThemeManager (www.delphi-gems.de).
    This only works with JvExVCL derived classes. }
  IJvDenySubClassing = interface
    ['{76942BC0-2A6E-4DC4-BFC9-8E110DB7F601}']
  end;


type
  TJvExControl = class(TControl, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  
  end;
  
  TJvExWinControl = class(TWinControl, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  

  TJvExGraphicControl = class(TGraphicControl, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  
  end;
  
  TJvExCustomControl = class(TCustomControl,  IJvWinControlEvents, IJvCustomControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  
  end;
  
  TJvExHintWindow = class(THintWindow,  IJvWinControlEvents, IJvCustomControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  
  end;
  


{$IFDEF VCL}
function ShiftStateToKeyData(Shift: TShiftState): Longint;

function InheritMsg(Instance: TControl; Msg: Integer; WParam, LParam: Integer): Integer; overload;
function InheritMsg(Instance: TControl; Msg: Integer): Integer; overload;
procedure DispatchMsg(Instance: TControl; var Msg);

procedure Control_ControlsListChanging(Instance: TControl; Control: TControl;
  Inserting: Boolean);
procedure Control_ControlsListChanged(Instance: TControl; Control: TControl;
  Inserting: Boolean);

{$IFNDEF COMPILER6_UP}
procedure TOpenControl_SetAutoSize(Instance: TControl; Value: Boolean);
{$ENDIF !COMPILER6_UP}
{$ENDIF VCL}

{$IFDEF VisualCLX}
function WidgetControl_Painting(Instance: TWidgetControl; Canvas: TCanvas;
  EventRegion: QRegionH): IInterface;
  // - returns NIL if the Instance is in csDestroying.
  // - enters the painting and returns an interface that leaves the painting when
  //   is is released.
procedure WidgetControl_DefaultPaint(Instance: TWidgetControl; Canvas: TCanvas);

function TWidgetControl_NeedKey(Instance: TWidgetControl; Key: Integer;
  Shift: TShiftState; const KeyText: WideString; InheritedValue: Boolean): Boolean;
{$ENDIF VisualCLX}

implementation

{$IFDEF VCL}

function ShiftStateToKeyData(Shift: TShiftState): Longint;
const
  AltMask = $20000000;
begin
  Result := 0;
  if ssAlt in Shift then
    Result := Result or AltMask;
end;

type
  TDisptachMethod = procedure(Self: TObject; var Msg: TMessage);

function InheritMsg(Instance: TControl; Msg: Integer; WParam, LParam: Integer): Integer;
var
  Proc: TDisptachMethod;
  Mesg: TMessage;
begin
  Mesg.Msg := Msg;
  Mesg.WParam := WParam;
  Mesg.LParam := LParam;
  Mesg.Result := 0;
  Proc := @TObject.Dispatch;
  Proc(Instance, Mesg);
  Result := Mesg.Result;
end;

function InheritMsg(Instance: TControl; Msg: Integer): Integer;
begin
  Result := InheritMsg(Instance, Msg, 0, 0);
end;

procedure DispatchMsg(Instance: TControl; var Msg);
var
  Temp: IJvDenySubClassing;
  IntfControl: IJvControlEvents;
  IntfWinControl: IJvWinControlEvents;
  PMsg: PMessage;
  CallInherited: Boolean;
  Canvas: TCanvas;
  DlgCodes: TDlgCodes;
begin
  CallInherited := True;
  PMsg := @Msg;

  if PMsg^.Msg = CM_DENYSUBCLASSING then
  begin
    PMsg^.Result := Ord(Instance.GetInterface(IJvDenySubClassing, Temp));
    Temp := nil; // does not destroy the control because it is derived from TComponent
   // Let the control handle CM_DENYSUBCLASSING the old way, too.
  end;

  { GetInterface is no problem because Instance is a TComponent derived class that
    is not released by an interface "Release". }
  if Instance.GetInterface(IJvControlEvents, IntfControl) then
  begin
    CallInherited := False;
    with IntfControl do
      case PMsg^.Msg of
        CM_VISIBLECHANGED:
          VisibleChanged;
        CM_ENABLEDCHANGED:
          EnabledChanged;
        CM_FONTCHANGED:
          FontChanged;
        CM_COLORCHANGED:
          ColorChanged;
        CM_PARENTFONTCHANGED:
          ParentFontChanged;
        CM_PARENTCOLORCHANGED:
          ParentColorChanged;
        CM_PARENTSHOWHINTCHANGED:
          ParentShowHintChanged;
        CM_TEXTCHANGED:
          TextChanged;
        CM_HINTSHOW:
          PMsg^.Result := Integer(HintShow(TCMHintShow(PMsg^).HintInfo^));
        CM_HITTEST:
          with TCMHitTest(PMsg^) do
            Result := Integer(HitTest(XPos, YPos));
        CM_MOUSEENTER:
            MouseEnter(TControl(PMsg^.LParam));
        CM_MOUSELEAVE:
            MouseLeave(TControl(PMsg^.LParam));
        CM_DIALOGCHAR:
          with TCMDialogChar(PMsg^) do
            Result := Ord(WantKey(CharCode, KeyDataToShiftState(KeyData), WideChar(CharCode)));

        WM_ERASEBKGND:
          begin
            Canvas := TCanvas.Create;
            try
              Canvas.Handle := HDC(PMsg^.WParam);
              PMsg^.Result := Ord(DoPaintBackground(Canvas, PMsg^.LParam));
            finally
              Canvas.Handle := 0;
              Canvas.Free;
            end;
          end
      else
        CallInherited := True;
      end;
  end;

  if CallInherited then
  begin
    if Instance.GetInterface(IJvWinControlEvents, IntfWinControl) then
    begin
      CallInherited := False;
      with IntfWinControl do
        case PMsg^.Msg of
          CM_CURSORCHANGED:
            CursorChanged;
          CM_SHOWINGCHANGED:
            ShowingChanged;
          CM_SHOWHINTCHANGED:
            ShowHintChanged;
          CM_CONTROLLISTCHANGE:
            if PMsg^.LParam <> 0 then
              ControlsListChanging(TControl(PMsg^.WParam), True)
            else
              ControlsListChanged(TControl(PMsg^.WParam), False);
          CM_CONTROLCHANGE:
            if PMsg^.LParam = 0 then
              ControlsListChanging(TControl(PMsg^.WParam), False)
            else
              ControlsListChanged(TControl(PMsg^.WParam), True);

          WM_GETDLGCODE:
            begin
              PMsg^.Result := InheritMsg(Instance, PMsg^.Msg, PMsg^.WParam, PMsg^.LParam);

              DlgCodes := [dcNative];
              if PMsg^.Result and DLGC_WANTARROWS <> 0 then
                Include(DlgCodes, dcWantArrows);
              if PMsg^.Result and DLGC_WANTTAB <> 0 then
                Include(DlgCodes, dcWantTab);
              if PMsg^.Result and DLGC_WANTALLKEYS <> 0 then
                Include(DlgCodes, dcWantAllKeys);
              if PMsg^.Result and DLGC_WANTCHARS <> 0 then
                Include(DlgCodes, dcWantChars);
              if PMsg^.Result and DLGC_BUTTON <> 0 then
                Include(DlgCodes, dcButton);

              DoGetDlgCode(DlgCodes);

              if not (dcNative in DlgCodes) then
              begin
                PMsg^.Result := 0;
                if dcWantAllKeys in DlgCodes then
                  PMsg^.Result := PMsg^.Result or DLGC_WANTALLKEYS;
                if dcWantArrows in DlgCodes then
                  PMsg^.Result := PMsg^.Result or DLGC_WANTARROWS;
                if dcWantTab in DlgCodes then
                  PMsg^.Result := PMsg^.Result or DLGC_WANTTAB;
                if dcWantChars in DlgCodes then
                  PMsg^.Result := PMsg^.Result or DLGC_WANTCHARS;
                if dcButton in DlgCodes then
                  PMsg^.Result := PMsg^.Result or DLGC_BUTTON;
              end;
            end;
        else
          CallInherited := True;
        end;
    end;
  end;

  if CallInherited then
    PMsg^.Result := InheritMsg(Instance, PMsg^.Msg, PMsg^.WParam, PMsg^.LParam);
end;

{ VCL sends CM_CONTROLLISTCHANGE and CM_CONTROLCHANGE in an other order that
  the CLX methods are used. So we must correct it by evaluating "Inserting". } 
procedure Control_ControlsListChanging(Instance: TControl; Control: TControl;
  Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Instance, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Instance, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure Control_ControlsListChanged(Instance: TControl; Control: TControl;
  Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Instance, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Instance, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$ENDIF VCL}

{$IFDEF VisualCLX}

type
  TOpenWidgetControl = class(TWidgetControl);

  TWidgetControlPainting = class(TInterfacedObject)
  private
    FCanvas: TCanvas;
    FInstance: TWidgetControl;
  public
    constructor Create(Instance: TWidgetControl; Canvas: TCanvas;
      EventRegion: QRegionH);
    destructor Destroy; override;
  end;

constructor TWidgetControlPainting.Create(Instance: TWidgetControl; Canvas: TCanvas;
  EventRegion: QRegionH);
begin
  inherited Create;
  FCanvas := Canvas;
  FInstance := Instance;

  TControlCanvas(FCanvas).StartPaint;
  QPainter_setClipRegion(FCanvas.Handle, EventRegion);
end;

destructor TWidgetControlPainting.Destroy;
begin
  TControlCanvas(FCanvas).StopPaint;
  inherited Destroy;
end;

function WidgetControl_Painting(Instance: TWidgetControl; Canvas: TCanvas;
  EventRegion: QRegionH): IInterface;
begin
  if csDestroying in Instance.ComponentState then
    Result := nil
  else
    Result := TWidgetControlPainting.Create(Instance, Canvas, EventRegion);
end;

procedure WidgetControl_DefaultPaint(Instance: TWidgetControl; Canvas: TCanvas);
var
  Event: QPaintEventH;
begin
  if not (csDestroying in Instance.ComponentState) and
     (not Supports(Instance, IJvCustomControlEvents) then
       { TCustomControls do not have a default paint method. }
  begin
    Event := QPaintEvent_create(QPainter_clipRegion(Canvas.Handle), False);
    try
      Instance.ControlState := Instance.ControlState + [csWidgetPainting];
      try
        QObject_event(Instance.Handle, Event);
      finally
        Instance.ControlState := Instance.ControlState - [csWidgetPainting];
      end;
    finally
      QPaintEvent_destroy(Event);
    end;
  end;
end;

function TWidgetControl_NeedKey(Instance: TWidgetControl; Key: Integer;
  Shift: TShiftState; const KeyText: WideString; InheritedValue: Boolean): Boolean;

  function IsTabKey: Boolean;
  begin
    Result := (Key = Key_Tab) or (Key = Tab_BackTab);
  end;

  function IsArrowKey: Boolean;
  begin
    Result := (Key = Key_Left) or (Key = Key_Right) or
              (Key = Key_Down) or (Key = Key_Up);
  end

var
  DlgCodes: TDlgCodes;
  Value: TInputKeys;
begin
  Result := InheritedValue;
  Value := TOpenWidgetControl(Instance).InputKeys;

  DlgCodes := [dcNative];
  if ikAll in Value then
    Include(DlgCodes, dcWantAllKeys);
  if ikArrows in Value then
    Include(DlgCodes, dcWantArrows);
  if ikTabs in Value then
    Include(DlgCodes, dcWantTab);
  if ikChars in Value then
    Include(DlgCodes, dcWantChars);

  DoGetDlgCode(DlgCodes);

  if not (dcNative in DlgCodes) then
  begin
    Result := False;
    if dcWantAllKeys in DlgCodes then
      Result := True;
    if (not Result) and (dcWantTab in DlgCodes) then
      Result := IsTabKey;
    if (not Result) and (dcWantArrows in DlgCodes) then
      Result := IsArrowKey;
    if (not Result) and (dcWantChars in DlgCodes) then
      Result := ((Shift * [ssCtrl, ssAlt] = []) and
                ((Hi(Word(Key)) = 0) or (Length(KeyText) > 0)) and
                not (IsTabKey or IsArrowKey);
  end;
end;

{$ENDIF VisualCLX}

// *****************************************************************************

{$IFDEF VCL}
procedure TJvExControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExControl.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
function TJvExControl.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

constructor TJvExControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExControl.Destroy;
begin
  
  inherited Destroy;
end;
{$IFDEF VCL}
procedure TJvExWinControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExWinControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExWinControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExWinControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExWinControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExWinControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExWinControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExWinControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExWinControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExWinControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExWinControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExWinControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExWinControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExWinControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExWinControl.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExWinControl.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExWinControl.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExWinControl.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExWinControl.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExWinControl.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExWinControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExWinControl.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExWinControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExWinControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExWinControl.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExWinControl.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExWinControl.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExWinControl.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExWinControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExWinControl.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExWinControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExWinControl.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}

{$IFDEF VCL}
procedure TJvExGraphicControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExGraphicControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExGraphicControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExGraphicControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExGraphicControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExGraphicControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExGraphicControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExGraphicControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExGraphicControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExGraphicControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExGraphicControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExGraphicControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExGraphicControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExGraphicControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExGraphicControl.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExGraphicControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExGraphicControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
function TJvExGraphicControl.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

constructor TJvExGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExGraphicControl.Destroy;
begin
  
  inherited Destroy;
end;
{$IFDEF VCL}
procedure TJvExCustomControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExCustomControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomControl.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExCustomControl.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomControl.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomControl.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomControl.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExCustomControl.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomControl.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExCustomControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExCustomControl.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExCustomControl.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExCustomControl.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExCustomControl.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

constructor TJvExCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExCustomControl.Destroy;
begin
  
  inherited Destroy;
end;
{$IFDEF VCL}
procedure TJvExHintWindow.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExHintWindow.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExHintWindow.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExHintWindow.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExHintWindow.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExHintWindow.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExHintWindow.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExHintWindow.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExHintWindow.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExHintWindow.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExHintWindow.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExHintWindow.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExHintWindow.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExHintWindow.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExHintWindow.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExHintWindow.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExHintWindow.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExHintWindow.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExHintWindow.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExHintWindow.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExHintWindow.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExHintWindow.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExHintWindow.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExHintWindow.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExHintWindow.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExHintWindow.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExHintWindow.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExHintWindow.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

constructor TJvExHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExHintWindow.Destroy;
begin
  
  inherited Destroy;
end;

// *****************************************************************************

{$IFNDEF COMPILER6_UP}
type
  TOpenControl = class(TControl);
  PBoolean = ^Boolean;
  PPointer = ^Pointer;

  TJumpCode = packed record
    Pop: Byte; // pop xxx
    Jmp: Byte; // jmp Offset
    Offset: Integer;
  end;

  TRelocationRec = packed record
    Jump: Word;
    Address: PPointer;
  end;

var
  AutoSizeOffset: Cardinal;
  TControl_SetAutoSize: Pointer;
  SavedControlCode: TJumpCode;

procedure TOpenControl_SetAutoSize(Instance: TControl; Value: Boolean);
begin
  with TOpenControl(Instance) do
  begin
    if AutoSize <> Value then
    begin
      PBoolean(Cardinal(Instance) + AutoSizeOffset)^ := Value;
      if Value then
        AdjustSize;
    end;
  end;
end;

procedure SetAutoSizeHook(Instance: TControl; Value: Boolean);
var
  IntfControl: IJvControlEvents;
begin
  if Instance.GetInterface(IJvControlEvents, IntfControl) then
    IntfControl.SetAutoSize(Value)
  else
    TOpenControl_SetAutoSize(Instance, Value);
end;

function ReadProtectedMemory(Address: Pointer; var Buffer; Count: Cardinal): Boolean;
var
  n: Cardinal;
begin
  Result := ReadProcessMemory(GetCurrentProcess, Address, @Buffer, Count, n);
  Result := Result and (n = Count);
end;

function WriteProtectedMemory(Address: Pointer; const Buffer; Count: Cardinal): Boolean;
var
  n: Cardinal;
begin
  Result := WriteProcessMemory(GetCurrentProcess, Address, @Buffer, Count, n);
  Result := Result and (n = Count);
end;

{$OPTIMIZATION ON} // be sure to have optimization activated
function GetCode(Instance: TOpenControl): Boolean;
begin
  { generated code:
      8A40xx       mov al,[eax+Byte(Offset)]
  }
  Result := Instance.AutoSize;
end;

procedure SetCode(Instance: TOpenControl);
begin
  { generated code:
      B201         mov dl,$01
      E8xxxxxxxx   call TControl.SetAutoSize
  }
  Instance.AutoSize := True;
end;

type
  PGetCodeRec = ^TGetCodeRec;
  TGetCodeRec = packed record
    Sign: Word; // $408a   bytes swapped
    Offset: Byte;
  end;

type
  PSetCodeRec = ^TSetCodeRec;
  TSetCodeRec = packed record
    Sign1: Word; // $01b2  bytes swapped
    Sign2: Byte; // $e8
    Offset: Integer;
  end;

const
  GetCodeSign = $408a;
  SetCodeSign1 = $01b2;
  SetCodeSign2 = $e8;

procedure InitHookVars;
var
  Relocation: TRelocationRec;
  PGetCode: PGetCodeRec;
  PSetCode: PSetCodeRec;
  Data: Byte;
begin
  TControl_SetAutoSize := nil;
  AutoSizeOffset := 0;

  PGetCode := @GetCode;
  PSetCode := @SetCode;

  if (PGetCode^.Sign = GetCodeSign) and
     (PSetCode^.Sign1 = SetCodeSign1) and (PSetCode^.Sign2 = SetCodeSign2) then
  begin
    AutoSizeOffset := PGetCode^.Offset;
    TControl_SetAutoSize := Pointer(Integer(@SetCode) + SizeOf(TSetCodeRec) +
      PSetCode^.Offset);

   // the relocation table meight be protected
    if ReadProtectedMemory(TControl_SetAutoSize, Data, SizeOf(Data)) then
    begin
      if Data = $FF then // TControl.SetAutoSize is in a dll or package
        if ReadProtectedMemory(TControl_SetAutoSize, Relocation, SizeOf(Relocation)) then
          TControl_SetAutoSize := Relocation.Address^;
    end;
  end;
end;

procedure InstallSetAutoSizeHook;
var
  Code: TJumpCode;
begin
  InitHookVars;
  if Assigned(TControl_SetAutoSize) then
  begin
    if PByte(TControl_SetAutoSize)^ = $53 then // push ebx
      Code.Pop := $5B // pop ebx
    else
    if PByte(TControl_SetAutoSize)^ = $55 then // push ebp
      Code.Pop := $5D // pop ebp
    else
      Exit;
    Code.Jmp := $E9;
    Code.Offset := Integer(@SetAutoSizeHook) -
      (Integer(TControl_SetAutoSize) + 1) - SizeOf(Code);

    if ReadProtectedMemory(Pointer(Cardinal(TControl_SetAutoSize) + 1),
      SavedControlCode, SizeOf(SavedControlCode)) then
    begin
     { The strange thing is that something overwrites the $e9 with a "PUSH xxx" }
      if WriteProtectedMemory(Pointer(Cardinal(TControl_SetAutoSize) + 1), Code,
           SizeOf(Code)) then
        FlushInstructionCache(GetCurrentProcess, TControl_SetAutoSize, SizeOf(Code));
    end;
  end;
end;

procedure UninstallSetAutoSizeHook;
var
  P: procedure;
  n: Cardinal;
begin
  P := TControl_SetAutoSize;
  if Assigned(P) then
  begin
    if WriteProcessMemory(GetCurrentProcess, Pointer(Cardinal(@P) + 1),
         @SavedControlCode, SizeOf(SavedControlCode), n) then
      FlushInstructionCache(GetCurrentProcess, @P, SizeOf(SavedControlCode));
  end;
end;

initialization
  InstallSetAutoSizeHook;

finalization
  UninstallSetAutoSizeHook;

{$ENDIF !COMPILER6_UP}

end.
