{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThreadDialog.PAS, released on 2004-12-06.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott de]
All Rights Reserved.

Contributor(s): Jens Fudickar [jens dott fudickar att oratool dott de].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvThreadDialog;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Forms, ExtCtrls, Buttons, StdCtrls,
  {$IFDEF MSWINDOWS}
  Windows, Controls,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QWindows, QControls,
  {$ENDIF UNIX}
  JvTypes, JvComponent, ComCtrls, JvResources;

type

  TJvCustomThreadDialog= class;
  tJvThreadBaseDialogOptions = Class (TPersistent)
  private
    fEnableCancelButton : Boolean;
    fShowDialog : Boolean;
    fShowModal : Boolean;
    fShowCancelButton : Boolean;
    fShowElapsedTime : Boolean;
    fInfoText : String;
    fCaption : String;
    fCancelButtonCaption : String;
    fOwner : TJvCustomThreadDialog;
  protected
  public
    constructor create (aOwner : TJvCustomThreadDialog); virtual;
  published
    property EnableCancelButton : Boolean read fEnableCancelButton write fEnableCancelButton default true;
    property ShowDialog : Boolean read fShowDialog write fShowDialog default false;
    property ShowModal : Boolean read fShowModal write fShowModal default true;
    property ShowCancelButton : Boolean read fShowCancelButton write fShowCancelButton default true;
    property ShowElapsedTime : Boolean read fShowElapsedTime write fShowElapsedTime default true;
    property InfoText : String read fInfoText write fInfoText;
    property Caption : String read fCaption write fCaption;
    property CancelButtonCaption : String read fCancelButtonCaption write fCancelButtonCaption;
  end;

  TJvCustomThreadDialogForm = class(TForm)
  private
    fConnectedThread : TComponent;
  protected
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SetConnectedThread(Value : TComponent);
  public
    property ConnectedThread : TComponent read fConnectedThread write SetConnectedThread;
  published
  end;

  TJvCustomThreadDialog = class(TJvComponent)
  private
    fDialogOptions : tJvThreadBaseDialogOptions;
  protected
    function CreateDialogOptions : tJvThreadBaseDialogOptions; virtual; abstract;
    procedure TransferThreadDialogOptions; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateThreadDialogForm (ConnectedThread : TComponent) : TJvCustomThreadDialogForm; virtual; abstract;

  published
    property DialogOptions : tJvThreadBaseDialogOptions read fDialogOptions write fDialogOptions;
  end;

  TJvThreadSimpleDialog = class(TJvCustomThreadDialog)
  private
    ThreadStatusDialog: TJvCustomThreadDialogForm;
    CancelButtonPanel: TPanel;
    CancelBtn: TButton;
    InfoTextPanel: TPanel;
    InfoText: TStaticText;
    TimeTextPanel: TPanel;
    TimeText: TStaticText;
    MainTimer: TTimer;
    Counter : Integer;
    StartTime: tDateTime;
  protected
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CancelBtnClick(Sender: TObject);
    procedure MainTimerTimer(Sender: TObject);
    procedure SetFormData;
    procedure SetFormHeightWidth;

    function CreateDialogOptions : tJvThreadBaseDialogOptions; override;
  public
    function CreateThreadDialogForm (ConnectedThread : TComponent) : TJvCustomThreadDialogForm; override;
  published
  end;

  tJvThreadAnimateDialogOptions = Class (tJvThreadBaseDialogOptions)
  private
    fCommonAVI : TCommonAVI;
    fFileName  : String;
  protected
  public
  published
    property CommonAVI : TCommonAVI read fCommonAVI write fCommonAVI;
    property FileName : String read fFileName write fFileName;
  end;

  TJvThreadAnimateDialog = class(TJvCustomThreadDialog)
  private
    ThreadStatusDialog: TJvCustomThreadDialogForm;
    CancelButtonPanel: TPanel;
    CancelBtn: TButton;
    AnimatePanel: TPanel;
    Animate: TAnimate;
    InfoTextPanel: TPanel;
    InfoText: TStaticText;
    TimeTextPanel: TPanel;
    TimeText: TStaticText;
    MainTimer: TTimer;
    Counter : Integer;
    StartTime: tDateTime;
  protected
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CancelBtnClick(Sender: TObject);
    procedure MainTimerTimer(Sender: TObject);
    procedure SetFormData;
    procedure SetFormHeightWidth;

    function GetDialogOptions : tJvThreadAnimateDialogOptions;

    function CreateDialogOptions : tJvThreadBaseDialogOptions; override;
  public
    function CreateThreadDialogForm (ConnectedThread : TComponent) : TJvCustomThreadDialogForm; override;
  published
//    property DialogOptions : tJvThreadAnimateDialogOptions read GetDialogOptions;
  end;


implementation

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning,
{$ENDIF UNITVERSIONING}
  JvThread;

function JvThreadComp (AComp : TComponent) : TJvThread ;
begin
  if aComp is TJvThread then
    Result := TJvThread(aComp)
  else
    Result := nil;
end;

//=== { tJvThreadBaseDialogOptions } ==========================================================

constructor tJvThreadBaseDialogOptions.create (aOwner : TJvCustomThreadDialog);
begin
  inherited Create;
  fOwner := aOwner;
  EnableCancelButton := true;
  ShowDialog := false;
  ShowModal := true;
  ShowCancelButton := true;
  ShowElapsedTime := true;
  CancelButtonCaption := RsButtonCancelCaption;
end;


//=== { TJvCustomThreadDialog } ==========================================================

procedure TJvCustomThreadDialogForm.SetConnectedThread(Value : TComponent);
begin
  if not (Value is TJvThread) then
    raise Exception.Create ('TJvCustomThreadDialogForm.SetConnectedThread : AThread must be a TJvThread-Component');
  fConnectedThread := Value;
end;

procedure TJvCustomThreadDialogForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
//  CanClose := fTerminated OR (NOT CurrentThread.IsRunning);
  CanClose := JvThreadComp(fConnectedThread).Terminated;
end;

//=== { TJvCustomThreadDialog } ==========================================================

constructor TJvCustomThreadDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDialogOptions := CreateDialogOptions;
end;

destructor TJvCustomThreadDialog.Destroy;
begin
  fDialogOptions.Free;
  inherited Destroy;
end;

procedure TJvCustomThreadDialog.TransferThreadDialogOptions;
begin
end;


//=== { TJvThreadSimpleDialog } ==========================================================

function TJvThreadSimpleDialog.CreateDialogOptions : tJvThreadBaseDialogOptions;
begin
  Result := tJvThreadBaseDialogOptions.Create (self);
end;

function TJvThreadSimpleDialog.CreateThreadDialogForm (ConnectedThread : TComponent) : TJvCustomThreadDialogForm;
begin
  Result := nil;
  if DialogOptions.ShowDialog then
  begin
    ThreadStatusDialog:= TJvCustomThreadDialogForm.CreateNew(Self);
    ThreadStatusDialog.ConnectedThread := ConnectedThread;
    Result := ThreadStatusDialog;
    CancelButtonPanel := TPanel.Create(ThreadStatusDialog);
    CancelBtn := TBitBtn.Create(ThreadStatusDialog);
    InfoTextPanel := TPanel.Create(ThreadStatusDialog);
    InfoText := TStaticText.Create(ThreadStatusDialog);
    TimeTextPanel := TPanel.Create(ThreadStatusDialog);
    TimeText := TStaticText.Create(ThreadStatusDialog);
    MainTimer := TTimer.Create(ThreadStatusDialog);
    with ThreadStatusDialog do
    begin
      BorderIcons := [];
      BorderStyle := bsDialog;
      Caption := ' ';
      ClientHeight := 88;
      ClientWidth := 268;
      FormStyle := fsStayOnTop;
      OldCreateOrder := False;
      Position := poScreenCenter;
      OnClose := FormClose;
      OnCloseQuery := FormCloseQuery;
      OnShow := FormShow;
      PixelsPerInch := 96;
    end;
    with InfoTextPanel do
    begin
      Top := 0;
      Caption := '';
      Parent := ThreadStatusDialog;
      Align := alTop;
      BevelOuter := bvNone;
      BorderWidth := 3;
    end;
    with InfoText do
    begin
      Parent := InfoTextPanel;
      Height := 22;
      InfoTextPanel.Height := InfoText.Height+6;
      Align := alClient;
      AutoSize := False;
      BevelInner := bvNone;
      BevelOuter := bvNone;
      ParentFont := False;
    end;
    with TimeTextPanel do
    begin
      Top := InfoTextPanel.Top+InfoTextPanel.Height+1;
      Parent := ThreadStatusDialog;
      Align := alTop;
      BevelOuter := bvNone;
      BorderWidth := 3;
    end;
    with TimeText do
    begin
      Height := 22;
      TimeTextPanel.Height := TimeText.Height+6;
      Caption := '';
      Parent := TimeTextPanel;
      Align := alClient;
      Alignment := taCenter;
      AutoSize := False;
      BevelInner := bvLowered;
      BorderStyle := sbsSunken;
      ParentFont := False;
    end;
    with CancelButtonPanel do
    begin
      Top := TimeTextPanel.Top+TimeTextPanel.Height+1;
      Caption := '';
      Parent := ThreadStatusDialog;
      Align := alTop;
      BevelOuter := bvNone;
    end;
    with CancelBtn do
    begin
      Parent := CancelButtonPanel;
      Anchors := [akTop];
      Caption := DialogOptions.CancelButtonCaption;
      OnClick := CancelBtnClick;
      Top := 0;
      CancelButtonPanel.Height := CancelBtn.Height+3;
    end;
    SetFormData;
    with MainTimer do
    begin
      Interval := 500;
      OnTimer := MainTimerTimer;
    end;
    if DialogOptions.ShowModal then
      ThreadStatusDialog.ShowModal
    else
      ThreadStatusDialog.Show;
  end;
end;

procedure TJvThreadSimpleDialog.FormShow(Sender: TObject);
begin
  StartTime := Now;
  Counter := 0;
  MainTimerTimer(NIL);
  SetFormData;
end;

procedure TJvThreadSimpleDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MainTimer.OnTimer := nil;
  MainTimer.Enabled := False;
  Action := caFree;
end;

procedure TJvThreadSimpleDialog.CancelBtnClick(Sender: TObject);
begin
  if Assigned (JvThreadComp(ThreadStatusDialog.ConnectedThread)) then
    JvThreadComp(ThreadStatusDialog.ConnectedThread).Terminate;
end;

procedure TJvThreadSimpleDialog.MainTimerTimer(Sender: TObject);
begin
  if not (csDestroying in ComponentState) and
     Assigned(ThreadStatusDialog) and
     Assigned(ThreadStatusDialog.ConnectedThread) and
     Assigned(DialogOptions) then
    IF JvThreadComp(ThreadStatusDialog.ConnectedThread).Terminated THEN
      ThreadStatusDialog.Close
    else
    begin
      Counter := COunter + 1;
      CASE (Counter Mod 4) OF
        0 :  InfoText.Caption := DialogOptions.InfoText + ' | ';
        1 :  InfoText.Caption := DialogOptions.InfoText+ ' / ';
        2 :  InfoText.Caption := DialogOptions.InfoText+ ' --';
        3 :  InfoText.Caption := DialogOptions.InfoText+ ' \ ';
      end;
      TimeText.Caption := FormatDateTime('hh:nn:ss',Now-StartTime);
      SetFormData;
    end;
end;

procedure TJvThreadSimpleDialog.SetFormHeightWidth;
VAR w : Integer;
    h : Integer;
begin
  if Assigned(ThreadStatusDialog) then
  begin
    w := ThreadStatusDialog.Canvas.TextWidth(DialogOptions.InfoText)+80;
    IF w < 200 THEN
      w := 200;
    ThreadStatusDialog.ClientWidth := w;
    CancelBtn.Left := Round((CancelButtonPanel.Width-CancelBtn.Width)/2);
    h := InfoTextPanel.Height+6;
    IF TimeTextPanel.Visible THEN
      h := h + TimeTextPanel.Height;
    IF CancelButtonPanel.Visible THEN
      h := h + CancelButtonPanel.Height;
    IF ThreadStatusDialog.ClientHeight <> h THEN
      ThreadStatusDialog.ClientHeight := h;
  end;
end;

procedure TJvThreadSimpleDialog.SetFormData;
begin
  if Assigned(ThreadStatusDialog) and
     Assigned(DialogOptions) then
  begin
    ThreadStatusDialog.Caption := DialogOptions.Caption;
    TimeTextPanel.Visible := DialogOptions.ShowElapsedTime;
    CancelBtn.Enabled := DialogOptions.EnableCancelButton;
    CancelButtonPanel.Visible := DialogOptions.ShowCancelButton;
    SetFormHeightWidth;
  end;
end;


//=== { TJvThreadAnimateDialog } ==========================================================

function TJvThreadAnimateDialog.CreateDialogOptions : tJvThreadBaseDialogOptions;
begin
  Result := tJvThreadAnimateDialogOptions.Create (self);
end;

function TJvThreadAnimateDialog.GetDialogOptions : tJvThreadAnimateDialogOptions;
begin
  Result := tJvThreadAnimateDialogOptions(inherited DialogOptions);
end;

function TJvThreadAnimateDialog.CreateThreadDialogForm (ConnectedThread : TComponent) : TJvCustomThreadDialogForm;
begin
  Result := nil;
  if DialogOptions.ShowDialog then
  begin
    ThreadStatusDialog:= TJvCustomThreadDialogForm.CreateNew(Self);
    ThreadStatusDialog.ConnectedThread := ConnectedThread;
    Result := ThreadStatusDialog;
    CancelButtonPanel := TPanel.Create(ThreadStatusDialog);
    CancelBtn := TBitBtn.Create(ThreadStatusDialog);
    AnimatePanel := TPanel.Create(ThreadStatusDialog);
    Animate := TAnimate.Create(ThreadStatusDialog);
    InfoTextPanel := TPanel.Create(ThreadStatusDialog);
    InfoText := TStaticText.Create(ThreadStatusDialog);
    TimeTextPanel := TPanel.Create(ThreadStatusDialog);
    TimeText := TStaticText.Create(ThreadStatusDialog);
    MainTimer := TTimer.Create(ThreadStatusDialog);
    with ThreadStatusDialog do
    begin
      BorderIcons := [];
      BorderStyle := bsDialog;
      Caption := ' ';
      ClientHeight := 88;
      ClientWidth := 268;
      FormStyle := fsStayOnTop;
      OldCreateOrder := False;
      Position := poScreenCenter;
      OnClose := FormClose;
      OnCloseQuery := FormCloseQuery;
      OnShow := FormShow;
      PixelsPerInch := 96;
    end;
    with InfoTextPanel do
    begin
      Top := 0;
      Parent := ThreadStatusDialog;
      Align := alTop;
      BevelOuter := bvNone;
      BorderWidth := 3;
    end;
    with InfoText do
    begin
      Height := 22;
      Parent := InfoTextPanel;
      InfoTextPanel.Height := InfoText.Height+6;
      Align := alClient;
      AutoSize := False;
      BevelInner := bvNone;
      BevelOuter := bvNone;
      ParentFont := False;
    end;
    with AnimatePanel do
    begin
      Caption := '';
      Top := InfoTextPanel.Height+1;
      Parent := ThreadStatusDialog;
      Align := alTop;
      BevelOuter := bvNone;
      BorderWidth := 3;
    end;
    with Animate do
    begin
      Parent := AnimatePanel;
      Top := 0;
      Left := 0;
      AutoSize := True;
      CommonAvi := tJvThreadAnimateDialogOptions(DialogOptions).CommonAVI;
      FileName := tJvThreadAnimateDialogOptions(DialogOptions).FileName;
      AnimatePanel.Height := Height+6;
    end;
    with TimeTextPanel do
    begin
      Top := AnimatePanel.Top+AnimatePanel.Height+1;
      Caption := '';
      Parent := ThreadStatusDialog;
      Align := alTop;
      BevelOuter := bvNone;
      BorderWidth := 3;
    end;
    with TimeText do
    begin
      Height := 22;
      TimeTextPanel.Height := TimeText.Height+6;
      Parent := TimeTextPanel;
      Align := alClient;
      Alignment := taCenter;
      AutoSize := False;
      BevelInner := bvLowered;
      BorderStyle := sbsSunken;
      ParentFont := False;
    end;
    with CancelButtonPanel do
    begin
      Top := TimeTextPanel.Top+TimeText.Height+1;
      Caption := '';
      Parent := ThreadStatusDialog;
      Align := alTop;
      BevelOuter := bvNone;
    end;
    with CancelBtn do
    begin
      Top := 0;
      Parent := CancelButtonPanel;
      Anchors := [akTop];
      Caption := DialogOptions.CancelButtonCaption;
      OnClick := CancelBtnClick;
      CancelButtonPanel.Height := CancelBtn.Height+3;
    end;
    SetFormData;
    with MainTimer do
    begin
      Interval := 500;
      OnTimer := MainTimerTimer;
    end;
    if DialogOptions.ShowModal then
      ThreadStatusDialog.ShowModal
    else
      ThreadStatusDialog.Show;
  end;
end;

procedure TJvThreadAnimateDialog.FormShow(Sender: TObject);
begin
  StartTime := Now;
  Counter := 0;
  SetFormData;
  Animate.Active := True;
  MainTimerTimer(NIL);
end;

procedure TJvThreadAnimateDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MainTimer.OnTimer := nil;
  MainTimer.Enabled := False;
  Action := caFree;
end;

procedure TJvThreadAnimateDialog.CancelBtnClick(Sender: TObject);
begin
  if Assigned (JvThreadComp(ThreadStatusDialog.ConnectedThread)) then
    JvThreadComp(ThreadStatusDialog.ConnectedThread).Terminate;
end;

procedure TJvThreadAnimateDialog.MainTimerTimer(Sender: TObject);
begin
  if not (csDestroying in ComponentState) and
     Assigned(ThreadStatusDialog) and
     Assigned(ThreadStatusDialog.ConnectedThread) and
     Assigned(DialogOptions) then
    IF JvThreadComp(ThreadStatusDialog.ConnectedThread).Terminated THEN
      ThreadStatusDialog.Close
    else
      TimeText.Caption := FormatDateTime('hh:nn:ss',Now-StartTime);
end;

procedure TJvThreadAnimateDialog.SetFormHeightWidth;
VAR w : Integer;
    h : Integer;
begin
  h := 6;
  if Assigned(ThreadStatusDialog) then
  begin
    w := 200;
    if InfoTextPanel.Visible then
    begin
      w := ThreadStatusDialog.Canvas.TextWidth(DialogOptions.InfoText)+80;
      if ThreadStatusDialog.Canvas.TextWidth(DialogOptions.InfoText)+80 > w then
        w := ThreadStatusDialog.Canvas.TextWidth(DialogOptions.InfoText)+80;
      h := h + InfoTextPanel.Height;
    end;
    if AnimatePanel.Visible then
    begin
      if Animate.Width+20 > w then
        w := Animate.Width+20;
      h := h + Animate.Height;
    end;
    CancelBtn.Left := Round((w-CancelBtn.Width)/2);
    Animate.Left := Round((w-Animate.Width)/2);
    IF TimeTextPanel.Visible THEN
      h := h + TimeTextPanel.Height;
    IF CancelButtonPanel.Visible THEN
      h := h + CancelButtonPanel.Height;
    IF ThreadStatusDialog.ClientWidth <> w THEN
      ThreadStatusDialog.ClientWidth := w;
    IF ThreadStatusDialog.ClientHeight <> h THEN
      ThreadStatusDialog.ClientHeight := h;
  end;
end;

procedure TJvThreadAnimateDialog.SetFormData;
begin
  if Assigned(ThreadStatusDialog) and
     Assigned(DialogOptions) then
  begin
    InfoText.Caption := DialogOptions.InfoText;
    ThreadStatusDialog.Caption := DialogOptions.Caption;
    InfoTextPanel.Visible := DialogOptions.Infotext <> '';
    AnimatePanel.Visible := FileExists(Animate.Filename) or (Animate.CommonAVI <> aviNone);
    TimeTextPanel.Visible := DialogOptions.ShowElapsedTime;
    CancelBtn.Enabled := DialogOptions.EnableCancelButton;
    CancelButtonPanel.Visible := DialogOptions.ShowCancelButton;
    SetFormHeightWidth;
  end;
end;


{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.


