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

The Original Code is: JvExMask.pas, released on 2004-01-04

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

unit JvQExMask;
interface
uses
  
  
  Qt, QGraphics, QControls, QForms, QMask, Types, QWindows,
  
  Classes, SysUtils,
  JvQTypes, JvQThemes, JVQCLVer, JvQExControls;


 {$IF not declared(PatchedVCLX)}
  
 {$IFEND}


type
  TJvExCustomMaskEdit = class(TCustomMaskEdit,  IJvEditControlEvents, IJvWinControlEvents, IJvControlEvents)
  
  
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure ParentColorChanged; override;
  protected
    procedure BoundsChanged; override;
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  
  private
    FHintColor: TColor;
    FSavedHintColor: TColor;
    FMouseOver: Boolean;
    FOnParentColorChanged: TNotifyEvent;
  
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  
  protected
    procedure CMFocusChanged(var Msg: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure DoFocusChanged(Control: TWinControl); dynamic;

    property MouseOver: Boolean read FMouseOver write FMouseOver;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
  
  
    FAboutJVCLX: TJVCLAboutInfo;
  published
    property AboutJVCLX: TJVCLAboutInfo read FAboutJVCLX write FAboutJVCLX stored False;
  
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
    procedure DoSetFocus(FocusedWnd: HWND); dynamic;
    procedure DoKillFocus(FocusedWnd: HWND); dynamic;
    procedure DoBoundsChanged; dynamic;
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  
  private
    FClipboardCommands: TJvClipboardCommands;
    
    FEditRect: TRect; // EM_GETRECT
    procedure EMGetRect(var Msg: TMessage); message EM_GETRECT;
    procedure EMSetRect(var Msg: TMessage); message EM_SETRECT;
    
  protected
    procedure DoUndo; dynamic;
    procedure DoClearText; dynamic;
    procedure DoClipboardPaste; dynamic;
    procedure DoClipboardCopy; dynamic;
    procedure DoClipboardCut; dynamic;
    procedure SetClipboardCommands(const Value: TJvClipboardCommands); virtual;

    property ClipboardCommands: TJvClipboardCommands read FClipboardCommands
      write SetClipboardCommands default [caCopy..caUndo];
  
  public
    procedure Clear; override;
  
  private
    FBeepOnError: Boolean;
  protected
    procedure DoBeepOnError; dynamic;
    procedure SetBeepOnError(Value: Boolean); virtual;
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError default True;
  end;
  TJvExPubCustomMaskEdit = class(TJvExCustomMaskEdit)
  
  end;
  

  TJvExMaskEdit = class(TMaskEdit,  IJvEditControlEvents, IJvWinControlEvents, IJvControlEvents)
  
  
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure ParentColorChanged; override;
  protected
    procedure BoundsChanged; override;
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  
  private
    FHintColor: TColor;
    FSavedHintColor: TColor;
    FMouseOver: Boolean;
    FOnParentColorChanged: TNotifyEvent;
  
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  
  protected
    procedure CMFocusChanged(var Msg: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure DoFocusChanged(Control: TWinControl); dynamic;

    property MouseOver: Boolean read FMouseOver write FMouseOver;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
  
  
    FAboutJVCLX: TJVCLAboutInfo;
  published
    property AboutJVCLX: TJVCLAboutInfo read FAboutJVCLX write FAboutJVCLX stored False;
  
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
    procedure DoSetFocus(FocusedWnd: HWND); dynamic;
    procedure DoKillFocus(FocusedWnd: HWND); dynamic;
    procedure DoBoundsChanged; dynamic;
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  
  private
    FClipboardCommands: TJvClipboardCommands;
    
    FEditRect: TRect; // EM_GETRECT
    procedure EMGetRect(var Msg: TMessage); message EM_GETRECT;
    procedure EMSetRect(var Msg: TMessage); message EM_SETRECT;
    
  protected
    procedure DoUndo; dynamic;
    procedure DoClearText; dynamic;
    procedure DoClipboardPaste; dynamic;
    procedure DoClipboardCopy; dynamic;
    procedure DoClipboardCut; dynamic;
    procedure SetClipboardCommands(const Value: TJvClipboardCommands); virtual;

    property ClipboardCommands: TJvClipboardCommands read FClipboardCommands
      write SetClipboardCommands default [caCopy..caUndo];
  
  public
    procedure Clear; override;
  
  private
    FBeepOnError: Boolean;
  protected
    procedure DoBeepOnError; dynamic;
    procedure SetBeepOnError(Value: Boolean); virtual;
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError default True;
  end;
  TJvExPubMaskEdit = class(TJvExMaskEdit)
  
  end;
  

implementation

{ The CONSTRUCTOR_CODE macro is used to extend the constructor by the macro
  content. }




procedure TJvExCustomMaskEdit.MouseEnter(Control: TControl);
begin
  if (not FMouseOver) and not (csDesigning in ComponentState) then
  begin
    FMouseOver := True;
    FSavedHintColor := Application.HintColor;
    if FHintColor <> clNone then
      Application.HintColor := FHintColor;
  end;
  inherited MouseEnter(Control);
  {$IF not declared(PatchedVCLX)}
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
  {$IFEND}
end;

procedure TJvExCustomMaskEdit.MouseLeave(Control: TControl);
begin
  if FMouseOver then
  begin
    FMouseOver := False;
    Application.HintColor := FSavedHintColor;
  end;
  inherited MouseLeave(Control);
  {$IF not declared(PatchedVCLX)}
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
  {$IFEND}
end;

procedure TJvExCustomMaskEdit.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;
procedure TJvExCustomMaskEdit.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

function TJvExCustomMaskEdit.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;

procedure TJvExCustomMaskEdit.BoundsChanged;
begin
  inherited BoundsChanged;
  DoBoundsChanged;
end;

procedure TJvExCustomMaskEdit.CMFocusChanged(var Msg: TCMFocusChanged);
begin
  inherited;
  DoFocusChanged(Msg.Sender);
end;

procedure TJvExCustomMaskEdit.DoFocusChanged(Control: TWinControl);
begin
end;
procedure TJvExCustomMaskEdit.DoBoundsChanged;
begin
end;

procedure TJvExCustomMaskEdit.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

procedure TJvExCustomMaskEdit.DoSetFocus(FocusedWnd: HWND);
begin
end;

procedure TJvExCustomMaskEdit.DoKillFocus(FocusedWnd: HWND);
begin
end;

function TJvExCustomMaskEdit.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  
  
  Result := False; // Qt allways paints the background
  
end;



constructor TJvExCustomMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FBeepOnError := True;
  FClipboardCommands := [caCopy..caUndo];
end;

destructor TJvExCustomMaskEdit.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvExCustomMaskEdit.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;


procedure TJvExCustomMaskEdit.DoClearText;
begin
 // (ahuser) there is no caClear so we restrict it to caCut
  if caCut in ClipboardCommands then
    
    
    inherited Clear;
    
end;

procedure TJvExCustomMaskEdit.DoUndo;
begin
  if caUndo in ClipboardCommands then
    TCustomEdit_Undo(Self);
end;

procedure TJvExCustomMaskEdit.DoClipboardPaste;
begin
  if caPaste in ClipboardCommands then
    TCustomEdit_Paste(Self);
end;

procedure TJvExCustomMaskEdit.DoClipboardCopy;
begin
  if caCopy in ClipboardCommands then
    TCustomEdit_Copy(Self);
end;

procedure TJvExCustomMaskEdit.DoClipboardCut;
begin
  if caCut in ClipboardCommands then
    TCustomEdit_Cut(Self);
end;

procedure TJvExCustomMaskEdit.SetClipboardCommands(const Value: TJvClipboardCommands);
begin
  FClipboardCommands := Value;
end;



procedure TJvExCustomMaskEdit.Clear;
begin
  DoClearText;
end;

procedure TJvExCustomMaskEdit.EMGetRect(var Msg: TMessage);
begin
  if Msg.LParam <> 0 then
  begin
    if IsRectEmpty(FEditRect) then
    begin
      PRect(Msg.LParam)^ := ClientRect;
      if Self.BorderStyle = bsSingle then
        InflateRect(PRect(Msg.LParam)^, -2, -2);
    end
    else
      PRect(Msg.LParam)^ := FEditRect;
  end;
end;

procedure TJvExCustomMaskEdit.EMSetRect(var Msg: TMessage);
begin
  if Msg.LParam <> 0 then
    FEditRect := PRect(Msg.LParam)^
  else
    FEditRect := ClientRect;
  Invalidate;
end;




procedure TJvExCustomMaskEdit.DoBeepOnError;
begin
  if BeepOnError then
    SysUtils.Beep;
end;

procedure TJvExCustomMaskEdit.SetBeepOnError(Value: Boolean);
begin
  FBeepOnError := Value;
end;





procedure TJvExMaskEdit.MouseEnter(Control: TControl);
begin
  if (not FMouseOver) and not (csDesigning in ComponentState) then
  begin
    FMouseOver := True;
    FSavedHintColor := Application.HintColor;
    if FHintColor <> clNone then
      Application.HintColor := FHintColor;
  end;
  inherited MouseEnter(Control);
  {$IF not declared(PatchedVCLX)}
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
  {$IFEND}
end;

procedure TJvExMaskEdit.MouseLeave(Control: TControl);
begin
  if FMouseOver then
  begin
    FMouseOver := False;
    Application.HintColor := FSavedHintColor;
  end;
  inherited MouseLeave(Control);
  {$IF not declared(PatchedVCLX)}
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
  {$IFEND}
end;

procedure TJvExMaskEdit.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;
procedure TJvExMaskEdit.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

function TJvExMaskEdit.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;

procedure TJvExMaskEdit.BoundsChanged;
begin
  inherited BoundsChanged;
  DoBoundsChanged;
end;

procedure TJvExMaskEdit.CMFocusChanged(var Msg: TCMFocusChanged);
begin
  inherited;
  DoFocusChanged(Msg.Sender);
end;

procedure TJvExMaskEdit.DoFocusChanged(Control: TWinControl);
begin
end;
procedure TJvExMaskEdit.DoBoundsChanged;
begin
end;

procedure TJvExMaskEdit.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

procedure TJvExMaskEdit.DoSetFocus(FocusedWnd: HWND);
begin
end;

procedure TJvExMaskEdit.DoKillFocus(FocusedWnd: HWND);
begin
end;

function TJvExMaskEdit.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  
  
  Result := False; // Qt allways paints the background
  
end;



constructor TJvExMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FBeepOnError := True;
  FClipboardCommands := [caCopy..caUndo];
end;

destructor TJvExMaskEdit.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvExMaskEdit.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;


procedure TJvExMaskEdit.DoClearText;
begin
 // (ahuser) there is no caClear so we restrict it to caCut
  if caCut in ClipboardCommands then
    
    
    inherited Clear;
    
end;

procedure TJvExMaskEdit.DoUndo;
begin
  if caUndo in ClipboardCommands then
    TCustomEdit_Undo(Self);
end;

procedure TJvExMaskEdit.DoClipboardPaste;
begin
  if caPaste in ClipboardCommands then
    TCustomEdit_Paste(Self);
end;

procedure TJvExMaskEdit.DoClipboardCopy;
begin
  if caCopy in ClipboardCommands then
    TCustomEdit_Copy(Self);
end;

procedure TJvExMaskEdit.DoClipboardCut;
begin
  if caCut in ClipboardCommands then
    TCustomEdit_Cut(Self);
end;

procedure TJvExMaskEdit.SetClipboardCommands(const Value: TJvClipboardCommands);
begin
  FClipboardCommands := Value;
end;



procedure TJvExMaskEdit.Clear;
begin
  DoClearText;
end;

procedure TJvExMaskEdit.EMGetRect(var Msg: TMessage);
begin
  if Msg.LParam <> 0 then
  begin
    if IsRectEmpty(FEditRect) then
    begin
      PRect(Msg.LParam)^ := ClientRect;
      if Self.BorderStyle = bsSingle then
        InflateRect(PRect(Msg.LParam)^, -2, -2);
    end
    else
      PRect(Msg.LParam)^ := FEditRect;
  end;
end;

procedure TJvExMaskEdit.EMSetRect(var Msg: TMessage);
begin
  if Msg.LParam <> 0 then
    FEditRect := PRect(Msg.LParam)^
  else
    FEditRect := ClientRect;
  Invalidate;
end;




procedure TJvExMaskEdit.DoBeepOnError;
begin
  if BeepOnError then
    SysUtils.Beep;
end;

procedure TJvExMaskEdit.SetBeepOnError(Value: Boolean);
begin
  FBeepOnError := Value;
end;


 // undefine at file end
end.
