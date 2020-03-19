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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id: jvcl/run/JvThreadDialog.pas jfudickar date $

unit JvThreadDialog;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Forms, StdCtrls,
  {$IFDEF MSWINDOWS}
  Windows, Controls, ComCtrls,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QWindows,
  {$ENDIF UNIX}
  JvThread, JvDynControlEngine, JvDynControlEngineIntf;

type
  TJvThreadBaseDialogOptions = class;
  TJvChangeThreadDialogOptionsEvent = procedure(DialogOptions: TJvThreadBaseDialogOptions) of object;

  TJvThreadBaseDialogOptions = class(TJvCustomThreadDialogOptions)
  private
    FCancelButtonCaption: string;
    FCaption: string;
    FEnableCancelButton: Boolean;
    FInfoText: string;
    FInfoTextAlignment: TAlignment;
    FShowCancelButton: Boolean;
    FShowElapsedTime: Boolean;
  protected
    procedure SetCancelButtonCaption(Value: string);
    procedure SetCaption(Value: string);
    procedure SetEnableCancelButton(Value: Boolean);
    procedure SetInfoText(Value: string);
    procedure SetShowCancelButton(Value: Boolean);
    procedure SetShowElapsedTime(Value: Boolean);
  public
    constructor Create(AOwner: TJvCustomThreadDialog); override;
    procedure Assign(Source: TPersistent); override;
  published
    property CancelButtonCaption: string read FCancelButtonCaption write SetCancelButtonCaption;
    property Caption: string read FCaption write SetCaption;
    property EnableCancelButton: Boolean read FEnableCancelButton write SetEnableCancelButton default True;
    property InfoText: string read FInfoText write SetInfoText;
    property InfoTextAlignment: TAlignment read FInfoTextAlignment write FInfoTextAlignment default taLeftJustify;
    property ShowCancelButton: Boolean read FShowCancelButton write SetShowCancelButton default True;
    property ShowElapsedTime: Boolean read FShowElapsedTime write SetShowElapsedTime default True;
  end;

  TJvThreadBaseDialog = class(TJvCustomThreadDialog)
  private
    FChangeThreadDialogOptions: TJvChangeThreadDialogOptionsEvent;
  published
    property OnPressCancel;
    property ChangeThreadDialogOptions: TJvChangeThreadDialogOptionsEvent read FChangeThreadDialogOptions write
        FChangeThreadDialogOptions;
  end;

  TJvThreadAnimateDialogOptions = class(TJvThreadBaseDialogOptions)
  private
    FCommonAVI: TCommonAVI;
    FFileName: string;
    FResName: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property CommonAVI: TCommonAVI read FCommonAVI write FCommonAVI;
    property FileName: string read FFileName write FFileName;
    property ResName: string read FResName write FResName;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF RTL230_UP}
  TJvThreadAnimateDialog = class(TJvThreadBaseDialog)
  private
    function GetDialogOptions: TJvThreadAnimateDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadAnimateDialogOptions);
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; override;
  public
    function CreateThreadDialogForm(ConnectedThread: TJvThread): TJvCustomThreadDialogForm; override;
  published
    property DialogOptions: TJvThreadAnimateDialogOptions read GetDialogOptions write SetDialogOptions;
  end;

  TJvThreadSimpleDialogOptions = class(TJvThreadBaseDialogOptions)
  private
    FProgressBarMarquee: Boolean;
    FProgressBarMax: Integer;
    FProgressBarMin: Integer;
    FProgressBarPosition: Integer;
    FProgressBarSmooth: Boolean;
    FShowProgressBar: Boolean;
  public
    constructor Create(AOwner: TJvCustomThreadDialog); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ProgressBarMarquee: Boolean read FProgressBarMarquee write FProgressBarMarquee default False;
    property ProgressBarMax: Integer read FProgressBarMax write FProgressBarMax default 100;
    property ProgressBarMin: Integer read FProgressBarMin write FProgressBarMin default 0;
    property ProgressBarPosition: Integer read FProgressBarPosition write FProgressBarPosition default -1;
    property ProgressBarSmooth: Boolean read FProgressBarSmooth write FProgressBarSmooth default False;
    property ShowProgressBar: Boolean read FShowProgressBar write FShowProgressBar default False;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF RTL230_UP}
  TJvThreadSimpleDialog = class(TJvThreadBaseDialog)
  private
    function GetDialogOptions: TJvThreadSimpleDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadSimpleDialogOptions);
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; override;
  public
    function CreateThreadDialogForm(ConnectedThread: TJvThread): TJvCustomThreadDialogForm; override;
  published
    property DialogOptions: TJvThreadSimpleDialogOptions read GetDialogOptions write SetDialogOptions;
  end;

  TJvDynControlEngineThreadDialogForm = class(TJvCustomThreadDialogForm)
  private
    FDefaultBorderWidth: Integer;
    FDynControlEngine: TJvDynControlEngine;
    function GetDynControlEngine: TJvDynControlEngine;
    procedure SetDynControlEngine(const Value: TJvDynControlEngine);
  protected
    procedure CreateTextPanel(AOwner: TComponent; AParent: TWinControl; var Panel: TWinControl; var Text: TControl;
        TextAlignment: TAlignment; const BaseName: string);
    property DefaultBorderWidth: Integer read FDefaultBorderWidth write FDefaultBorderWidth;
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine write SetDynControlEngine;
  end;

  TJvThreadBaseDialogForm = class(TJvDynControlEngineThreadDialogForm)
  private
    FCancelBtn: TButton;
    FCancelButtonPanel: TWinControl;
    FChangeThreadDialogOptions: TJvChangeThreadDialogOptionsEvent;
    FInfoText: TControl;
    FInfoTextPanel: TWinControl;
    FMainPanel: TWinControl;
    FOrgInfoTextWidth: Integer;
    FStartTime: TDateTime;
    FTimeText: TControl;
    FTimeTextPanel: TWinControl;
    IInfoTextControlAutoSize: IJvDynControlAutoSize;
    IInfoTextControlCaption: IJvDynControlCaption;
    ITimeTextControlCaption: IJvDynControlCaption;
    function CalculateFormHeight: Integer; virtual;
    function CalculateFormWidth: Integer; virtual;
    procedure CreateControlCancelButton;
    procedure CreateControlInfoText;
    procedure CreateControlMainPanel;
    procedure CreateControlTimeText;
    procedure SetControlHeightWidth; virtual;
    procedure SetFormDefaultProperties;
    procedure SetFormInfoText; virtual;
    function GetDialogOptions: TJvThreadBaseDialogOptions;
    procedure SetDialogOptions(const Value: TJvThreadBaseDialogOptions);
    procedure SetFormHeightWidth;
  protected
    procedure FreeFormControls; override;
    procedure InitializeFormContents; override;
    procedure UpdateFormContents; override;
  public
    property DialogOptions: TJvThreadBaseDialogOptions read GetDialogOptions write SetDialogOptions;
    property ChangeThreadDialogOptions: TJvChangeThreadDialogOptionsEvent read
        FChangeThreadDialogOptions write FChangeThreadDialogOptions;
  end;

  TJvThreadSimpleDialogForm = class(TJvThreadBaseDialogForm)
  private
    FCounter: Integer;
    FProgressbar: TWinControl;
    FProgressbarPanel: TWinControl;
    IProgressBarControl : IJvDynControlProgressbar;
    function CalculateFormHeight: Integer; override;
    procedure CreateControlProgressBar;
    function GetDialogOptions: TJvThreadSimpleDialogOptions;
    procedure SetControlHeightWidth; override;
    procedure SetDialogOptions(Value: TJvThreadSimpleDialogOptions);
  protected
    procedure CreateFormControls; override;
    procedure FreeFormControls; override;
    procedure InitializeFormContents; override;
    procedure UpdateFormContents; override;
  public
    property DialogOptions: TJvThreadSimpleDialogOptions read GetDialogOptions write SetDialogOptions;
  end;

  TJvThreadAnimateDialogForm = class(TJvThreadBaseDialogForm)
  private
    FAnimate: TAnimate;
    FAnimatePanel: TWinControl;
    function CalculateFormHeight: Integer; override;
    function CalculateFormWidth: Integer; override;
    procedure CreateControlAnimate;
    function GetDialogOptions: TJvThreadAnimateDialogOptions;
    procedure SetControlHeightWidth; override;
    procedure SetDialogOptions(Value: TJvThreadAnimateDialogOptions);
  protected
    procedure CreateFormControls; override;
    procedure FreeFormControls; override;
    procedure InitializeFormContents; override;
    procedure UpdateFormContents; override;
  public
    property DialogOptions: TJvThreadAnimateDialogOptions read GetDialogOptions write SetDialogOptions;
  end;



{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: jvcl/run/JvThreadDialog.pas $';
    Revision: '$Revision: 9594babef19065116dda9ae3f8ea25b629c9e477 $';
    Date: '$Date: 2013-08-10 22:56:30 +0200 $';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Dialogs, Graphics,
  {$IFDEF COMPILER11_UP} // Delphi 2007 introduced ShellAnimations
  ShellAnimations,
  {$ENDIF COMPILER11_UP}
  JvResources;

function Max(a, b: Integer): Integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

//=== { TJvThreadBaseDialogOptions } =========================================

constructor TJvThreadBaseDialogOptions.Create(AOwner: TJvCustomThreadDialog);
begin
  inherited Create(AOwner);
  FEnableCancelButton := True;
  FShowCancelButton := True;
  FShowElapsedTime := True;
  FCancelButtonCaption := RsButtonCancelCaption;
  FInfoTextAlignment := taLeftJustify;
end;

procedure TJvThreadBaseDialogOptions.Assign(Source: TPersistent);
begin
  if Source is TJvThreadBaseDialogOptions then
  begin
    CancelButtonCaption := TJvThreadBaseDialogOptions(Source).CancelButtonCaption;
    Caption := TJvThreadBaseDialogOptions(Source).Caption;
    EnableCancelButton := TJvThreadBaseDialogOptions(Source).EnableCancelButton;
    InfoText := TJvThreadBaseDialogOptions(Source).InfoText;
    InfoTextAlignment := TJvThreadBaseDialogOptions(Source).InfoTextAlignment;
    ShowCancelButton := TJvThreadBaseDialogOptions(Source).ShowCancelButton;
    ShowElapsedTime := TJvThreadBaseDialogOptions(Source).ShowElapsedTime;
  end;
  Inherited Assign(Source);
end;

procedure TJvThreadBaseDialogOptions.SetCancelButtonCaption(Value: string);
begin
  FCancelButtonCaption := Value;
end;

procedure TJvThreadBaseDialogOptions.SetCaption(Value: string);
begin
  FCaption := Value;
end;

procedure TJvThreadBaseDialogOptions.SetEnableCancelButton(Value: Boolean);
begin
  FEnableCancelButton := Value;
end;

procedure TJvThreadBaseDialogOptions.SetInfoText(Value: string);
begin
  FInfoText := Value;
end;

procedure TJvThreadBaseDialogOptions.SetShowCancelButton(Value: Boolean);
begin
  FShowCancelButton := Value;
end;

procedure TJvThreadBaseDialogOptions.SetShowElapsedTime(Value: Boolean);
begin
  FShowElapsedTime := Value;
end;

//=== { TJvThreadSimpleDialog } ==============================================

function TJvThreadSimpleDialog.CreateDialogOptions: TJvCustomThreadDialogOptions;
begin
  Result := TJvThreadSimpleDialogOptions.Create(Self);
end;

function TJvThreadSimpleDialog.CreateThreadDialogForm(ConnectedThread: TJvThread): TJvCustomThreadDialogForm;
var
  ThreadDialogForm: TJvThreadSimpleDialogForm;
begin
  if DialogOptions.ShowDialog then
  begin
    ThreadDialogForm := TJvThreadSimpleDialogForm.CreateNewFormStyle(ConnectedThread, DialogOptions.FormStyle);
    ThreadDialogForm.DialogOptions := DialogOptions;
    ThreadDialogForm.OnPressCancel := OnPressCancel;
    ThreadDialogForm.ChangeThreadDialogOptions := ChangeThreadDialogOptions;
//    ThreadDialogForm.CreateFormControls;
    Result := ThreadDialogForm;
  end
  else
    Result := nil;
end;

function TJvThreadSimpleDialog.GetDialogOptions: TJvThreadSimpleDialogOptions;
begin
  Result := TJvThreadSimpleDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadSimpleDialog.SetDialogOptions(Value: TJvThreadSimpleDialogOptions);
begin
  inherited DialogOptions.Assign(Value);
end;

//=== { TJvThreadAnimateDialog } =============================================

function TJvThreadAnimateDialog.CreateDialogOptions: TJvCustomThreadDialogOptions;
begin
  Result := TJvThreadAnimateDialogOptions.Create(Self);
end;

function TJvThreadAnimateDialog.CreateThreadDialogForm(ConnectedThread: TJvThread):
TJvCustomThreadDialogForm;
var
  ThreadDialogForm: TJvThreadAnimateDialogForm;
begin
  if DialogOptions.ShowDialog then
  begin
    ThreadDialogForm := TJvThreadAnimateDialogForm.CreateNewFormStyle(ConnectedThread,
      DialogOptions.FormStyle);
    ThreadDialogForm.DialogOptions := DialogOptions;
    ThreadDialogForm.ChangeThreadDialogOptions := ChangeThreadDialogOptions;
    ThreadDialogForm.OnPressCancel := OnPressCancel;
//    ThreadDialogForm.CreateFormControls;
    Result := ThreadDialogForm;
  end
  else
    Result := nil;
end;

function TJvThreadAnimateDialog.GetDialogOptions: TJvThreadAnimateDialogOptions;
begin
  Result := TJvThreadAnimateDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadAnimateDialog.SetDialogOptions(Value: TJvThreadAnimateDialogOptions);
begin
  inherited DialogOptions.Assign(Value);
end;

//=== { TJvThreadSimpleDialogOptions } =======================================

constructor TJvThreadSimpleDialogOptions.Create(AOwner: TJvCustomThreadDialog);
begin
  inherited Create(AOwner);
  FShowProgressBar := False;
  FProgressBarPosition := -1;
  FProgressBarSmooth := False;
  FProgressBarMax := 100;
  FProgressBarMin := 0;
  FProgressBarMarquee := False;
end;

procedure TJvThreadSimpleDialogOptions.Assign(Source: TPersistent);
begin
  if Source is TJvThreadSimpleDialogOptions then
  begin
    ProgressBarMarquee := TJvThreadSimpleDialogOptions(Source).ProgressBarMarquee;
    ProgressBarMax := TJvThreadSimpleDialogOptions(Source).ProgressBarMax;
    ProgressBarMin := TJvThreadSimpleDialogOptions(Source).ProgressBarMin;
    ProgressBarPosition := TJvThreadSimpleDialogOptions(Source).ProgressBarPosition;
    ProgressBarSmooth := TJvThreadSimpleDialogOptions(Source).ProgressBarSmooth;
    ShowProgressBar := TJvThreadSimpleDialogOptions(Source).ShowProgressBar;
  end;
  Inherited Assign(Source);
end;

function TJvThreadSimpleDialogForm.CalculateFormHeight: Integer;
begin
  Result := inherited CalculateFormHeight;
  if Assigned(FProgressbarPanel) and FProgressbarPanel.Visible then
    Result := Result + FProgressbarPanel.Height;
end;

procedure TJvThreadSimpleDialogForm.CreateControlProgressBar;
var
  ITmpPanel: IJvDynControlPanel;
  ITmpAlign: IJvDynControlAlign;
begin
  FProgressbarPanel := DynControlEngine.CreatePanelControl(Self, FMainPanel, 'ProgressbarPanel', '', alTop);
  if not Supports(FProgressbarPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  ITmpPanel.ControlSetBorder(bvNone, bvNone, 0, bsNone, FDefaultBorderWidth);
  FProgressbar := DynControlEngine.CreateProgressbarControl(Self, FProgressbarPanel, 'Progressbar');
  Supports(FProgressbar, IJvDynControlProgressbar, IProgressBarControl);
  FProgressbarPanel.Height := FProgressbar.Height + FDefaultBorderWidth*2;
  if Supports(FProgressbar, IJvDynControlAlign, ITmpAlign) then
    ITmpAlign.ControlSetAlign(alClient);
end;

procedure TJvThreadSimpleDialogForm.CreateFormControls;
begin
  Inherited CreateFormControls;
  FDefaultBorderWidth := 3;
  CreateControlMainPanel;

  CreateControlInfoText;
  CreateControlTimeText;
  CreateControlProgressBar;

  CreateControlCancelButton;

  SetFormDefaultProperties;
  SetFormHeightWidth;
end;

procedure TJvThreadSimpleDialogForm.FreeFormControls;
begin
  if Assigned(IProgressBarControl) then
    IProgressBarControl.ControlSetMarquee(False);// To deactivate the toolbar marquee in rare circumstances
  IProgressBarControl := nil;
  inherited;
end;

procedure TJvThreadSimpleDialogForm.InitializeFormContents;
begin
  inherited InitializeFormContents;
  FCounter   := 0;
end;

procedure TJvThreadSimpleDialogForm.UpdateFormContents;
begin
  if (csDestroying in ComponentState) or not FormIsShown then
    Exit;
  inherited UpdateFormContents;
  if Assigned(DialogOptions) then
  begin
    if Assigned(FProgressbarPanel) then
      FProgressbarPanel.Visible := DialogOptions.ShowProgressBar;
    if Assigned(IProgressBarControl) then
    begin
      IProgressBarControl.ControlSetMin(DialogOptions.ProgressBarMin);
      IProgressBarControl.ControlSetMax(DialogOptions.ProgressBarMax);
      IProgressBarControl.ControlSetMarquee(DialogOptions.ProgressBarMarquee);
      if not DialogOptions.ProgressBarMarquee then
        if (DialogOptions.ProgressBarPosition >= DialogOptions.ProgressBarMin) and (DialogOptions.ProgressBarPosition <= DialogOptions.ProgressBarMax)  then
          IProgressBarControl.ControlSetPosition(DialogOptions.ProgressBarPosition)
        else
          IProgressBarControl.ControlSetPosition(((FCounter*10) mod (DialogOptions.ProgressBarMax-DialogOptions.ProgressBarMin)+10));
      IProgressBarControl.ControlSetSmooth(DialogOptions.ProgressBarSmooth);
    end;
    case FCounter mod 4 of
      0: Caption := DialogOptions.Caption + ' | ';
      1: Caption := DialogOptions.Caption + ' / ';
      2: Caption := DialogOptions.Caption + ' --';
    else
      Caption := DialogOptions.Caption + ' \ ';
    end;
    Inc (FCounter);
  end;
  SetFormHeightWidth;
end;

function TJvThreadSimpleDialogForm.GetDialogOptions: TJvThreadSimpleDialogOptions;
begin
  Result := TJvThreadSimpleDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadSimpleDialogForm.SetControlHeightWidth;
var
  h: Integer;
begin
  inherited SetControlHeightWidth;
  if Assigned(FProgressbarPanel) then
    FProgressbarPanel.Width := FTimeTextPanel.Width;
  h := FDefaultBorderWidth;
  if Assigned(FInfoTextPanel) and FInfoTextPanel.Visible then
  begin
    if FInfoTextPanel.Top <> h then
      FInfoTextPanel.Top := h;
    H := H + FInfoTextPanel.Height;
  end;
  if Assigned(FProgressbarPanel) and FProgressbarPanel.Visible then
  begin
    if FProgressbarPanel.Top <> h then
      FProgressbarPanel.Top := h;
    H := H + FProgressbarPanel.Height;
  end;
  if Assigned(FTimeTextPanel) and FTimeTextPanel.Visible then
  begin
    if FTimeTextPanel.Top <> h then
      FTimeTextPanel.Top := h;
    H := H + FTimeTextPanel.Height;
  end;
  if Assigned(FCancelButtonPanel) and FCancelButtonPanel.Visible then
    if FCancelButtonPanel.Top <> h then
      FCancelButtonPanel.Top := h;
end;

procedure TJvThreadSimpleDialogForm.SetDialogOptions(Value:
  TJvThreadSimpleDialogOptions);
begin
  inherited DialogOptions := Value;
end;

function TJvThreadAnimateDialogForm.CalculateFormHeight: Integer;
begin
  Result := inherited CalculateFormHeight;
  if Assigned(FAnimatePanel) and FAnimatePanel.Visible then
    Result := Result + FAnimatePanel.Height;
end;

function TJvThreadAnimateDialogForm.CalculateFormWidth: Integer;
var
  W: Integer;
begin
  W := Inherited CalculateFormWidth;

  if Assigned(FAnimatePanel) and FAnimatePanel.Visible then
    W := Max(W, FAnimate.Width + 20);

  Result := w;
end;

procedure TJvThreadAnimateDialogForm.CreateControlAnimate;
var
  ITmpPanel: IJvDynControlPanel;
begin
  FAnimatePanel := DynControlEngine.CreatePanelControl(Self, FMainPanel, 'AnimatePanel', '', alTop);
  if not Supports(FAnimatePanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  ITmpPanel.ControlSetBorder(bvNone, bvRaised, 0, bsNone, FDefaultBorderWidth);

  FAnimate := TAnimate.Create(Self);
  FAnimate.Parent := FAnimatePanel;
  FAnimate.Top := FDefaultBorderWidth;
  FAnimate.Left := FDefaultBorderWidth;
  FAnimate.AutoSize := True;
  FAnimate.CommonAVI := TJvThreadAnimateDialogOptions(DialogOptions).CommonAVI;
  FAnimate.FileName := TJvThreadAnimateDialogOptions(DialogOptions).FileName;
  FAnimate.ResName := TJvThreadAnimateDialogOptions(DialogOptions).ResName;
  FAnimatePanel.Height := FAnimate.Height + FDefaultBorderWidth*2;
end;

procedure TJvThreadAnimateDialogForm.CreateFormControls;
begin
  Inherited CreateFormControls;
  FDefaultBorderWidth := 3;
  CreateControlMainPanel;

  CreateControlInfoText;
  CreateControlAnimate;
  CreateControlTimeText;

  CreateControlCancelButton;

  SetFormDefaultProperties;
  SetFormHeightWidth;
end;

procedure TJvThreadAnimateDialogForm.FreeFormControls;
begin
  inherited;
end;

procedure TJvThreadAnimateDialogForm.InitializeFormContents;
begin
  inherited InitializeFormContents;
  FAnimate.Active := True;
end;

procedure TJvThreadAnimateDialogForm.UpdateFormContents;
begin
  if (csDestroying in ComponentState) or not FormIsShown then
    Exit;
  inherited UpdateFormContents;
  if Assigned(DialogOptions) then
  begin
    Caption := DialogOptions.Caption;
    FAnimatePanel.Visible := FileExists(FAnimate.FileName) or
      (FAnimate.CommonAVI <> aviNone) or (FAnimate.ResName <> '');
  end;
  SetFormHeightWidth;
end;

function TJvThreadAnimateDialogForm.GetDialogOptions: TJvThreadAnimateDialogOptions;
begin
  Result := TJvThreadAnimateDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadAnimateDialogForm.SetControlHeightWidth;
var h : Integer;
begin
  Inherited SetControlHeightWidth;
  FAnimate.Left   := (FAnimatePanel.Width - FAnimate.Width) div 2;
  FAnimatePanel.Height := FAnimate.Height + FDefaultBorderWidth*2;
  h := FDefaultBorderWidth;
  if Assigned(FInfoTextPanel) and FInfoTextPanel.Visible then
  begin
    if FInfoTextPanel.Top <> h then
      FInfoTextPanel.Top := h;
    H := H + FInfoTextPanel.Height;
  end;
  if Assigned(FAnimatePanel) and FAnimatePanel.Visible then
  begin
    if FAnimatePanel.Top <> h then
      FAnimatePanel.Top := h;
    H := H + FAnimatePanel.Height;
  end;
  if Assigned(FTimeTextPanel) and FTimeTextPanel.Visible then
  begin
    if FTimeTextPanel.Top <> h then
      FTimeTextPanel.Top := h;
    H := H + FTimeTextPanel.Height;
  end;
  if Assigned(FCancelButtonPanel) and FCancelButtonPanel.Visible then
    if FCancelButtonPanel.Top <> h then
      FCancelButtonPanel.Top := h;
end;

procedure TJvThreadAnimateDialogForm.SetDialogOptions(Value:
  TJvThreadAnimateDialogOptions);
begin
  inherited DialogOptions := Value;
end;

procedure TJvDynControlEngineThreadDialogForm.CreateTextPanel(AOwner: TComponent; AParent: TWinControl; var Panel:
    TWinControl; var Text: TControl; TextAlignment: TAlignment; const BaseName: string);
var
  ITmpPanel: IJvDynControlPanel;
  ITmpAlignment: IJvDynControlAlignment;
begin
  Panel := DynControlEngine.CreatePanelControl(AOwner, AParent,
    BaseName + 'Panel', '', alTop);
  if not Supports(Panel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  ITmpPanel.ControlSetBorder(bvNone, bvNone, 0, bsNone, FDefaultBorderWidth);
  Text := DynControlEngine.CreateLabelControl(AOwner,
    Panel, BaseName + 'StaticText', '', nil);
  Text.Top := FDefaultBorderWidth;
  Text.Left := FDefaultBorderWidth;
  if Supports(Text, IJvDynControlAlignment, ITmpAlignment) then
    ITmpAlignment.ControlSetAlignment(TextAlignment);
end;

function TJvDynControlEngineThreadDialogForm.GetDynControlEngine: TJvDynControlEngine;
begin
  Result := DefaultDynControlEngine;
end;

procedure TJvDynControlEngineThreadDialogForm.SetDynControlEngine(const Value: TJvDynControlEngine);
begin
  if not Assigned(Value) then
    FDynControlEngine := DefaultDynControlEngine
  else
    FDynControlEngine := Value;
end;

function TJvThreadBaseDialogForm.CalculateFormHeight: Integer;
var
  H: Integer;
begin
  H := 0;
  if Assigned(FInfoTextPanel) and FInfoTextPanel.Visible then
    H := FInfoTextPanel.Height;
  if Assigned(FTimeTextPanel) and FTimeTextPanel.Visible then
    H := H + FTimeTextPanel.Height;
  if Assigned(FCancelButtonPanel) and FCancelButtonPanel.Visible then
    H := H + FCancelButtonPanel.Height;
  Result := h;
end;

function TJvThreadBaseDialogForm.CalculateFormWidth: Integer;
var
  W: Integer;
begin
  if Assigned(FInfoTextPanel) and FInfoTextPanel.Visible then
    W := FOrgInfoTextWidth + 20
  else
    W := 0;
  W := Round(W/10)*10;
  if W < 250 then
    W := 250;
  if W > Screen.Width-100 then
    W := Screen.Width-100;
  Result := w;
end;

procedure TJvThreadBaseDialogForm.CreateControlCancelButton;
begin
  FCancelButtonPanel := DynControlEngine.CreatePanelControl(Self,
    FMainPanel, 'ButtonPanel', '', alTop);
  FCancelBtn := DynControlEngine.CreateButton(Self, FCancelButtonPanel,
    'CancelBtn', DialogOptions.CancelButtonCaption, '', DefaultCancelBtnClick,
    True, True);
  FCancelBtn.Anchors := [akTop];
  FCancelBtn.Top := FDefaultBorderWidth;
  FCancelButtonPanel.Height := FCancelBtn.Height + FDefaultBorderWidth*2;
end;

procedure TJvThreadBaseDialogForm.CreateControlInfoText;
begin
  CreateTextPanel(Self, FMainPanel, FInfoTextPanel, FInfoText,
    DialogOptions.InfoTextAlignment, 'Info');

  Supports(FInfoText, IJvDynControlCaption, IInfoTextControlCaption);
  Supports(FInfoText, IJvDynControlAutoSize, IInfoTextControlAutoSize);
end;

procedure TJvThreadBaseDialogForm.CreateControlMainPanel;
var
  ITmpPanel: IJvDynControlPanel;
begin
  FMainPanel := DynControlEngine.CreatePanelControl(Self, Self,
    'MainPanel', '', alClient);
  if not Supports(FMainPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  ITmpPanel.ControlSetBorder(bvNone, bvNone, 0, bsNone, FDefaultBorderWidth);
end;

procedure TJvThreadBaseDialogForm.CreateControlTimeText;
begin
  CreateTextPanel(Self, FMainPanel, FTimeTextPanel, FTimeText, taCenter, 'Time');
  Supports(FTimeText, IJvDynControlCaption, ITimeTextControlCaption);
end;

procedure TJvThreadBaseDialogForm.FreeFormControls;
begin
  IInfoTextControlAutoSize:= nil;
  IInfoTextControlCaption:= nil;
  ITimeTextControlCaption:= nil;
  inherited;
end;

procedure TJvThreadBaseDialogForm.InitializeFormContents;
begin
  inherited InitializeFormContents;
  SetFormHeightWidth;
  FStartTime := Now;
end;

procedure TJvThreadBaseDialogForm.SetControlHeightWidth;
begin
  if Assigned(FCancelButtonPanel) and Assigned(FCancelBtn) then
    FCancelBtn.Left := (FCancelButtonPanel.Width - FCancelBtn.Width) div 2;
  if Assigned(FInfoText) and Assigned(FInfoTextPanel) then
  begin
    FInfoText.Width := FInfoTextPanel.Width-FDefaultBorderWidth*2;
    FInfoTextPanel.Height := FInfoText.Height+FDefaultBorderWidth*2;
  end;
  if Assigned(FTimeText) and Assigned(FTimeTextPanel) then
  begin
    FTimeText.Width := FTimeTextPanel.Width-FDefaultBorderWidth*2;
    FTimeTextPanel.Height := FTimeText.Height+FDefaultBorderWidth*2;
  end;
end;

procedure TJvThreadBaseDialogForm.SetFormDefaultProperties;
begin
  BorderIcons := [];
  BorderStyle := bsDialog;
  Caption := ' ';
  FormStyle := DialogOptions.FormStyle;
  {$IFDEF COMPILER7_UP}
  Position := poOwnerFormCenter;
  {$ELSE}
  Position := poScreenCenter;
  {$ENDIF COMPILER7_UP};
end;

procedure TJvThreadBaseDialogForm.SetFormHeightWidth;
var
  H, W: Integer;
begin
  if (csDestroying in ComponentState) or not FormIsShown then
    Exit;
  w := CalculateFormWidth;
  W := Round(W/10)*10;
  if W < (ClientWidth -50) then // Reduces the resize flickering when the text is changed to often
    ClientWidth := W
  else if W > ClientWidth then
    ClientWidth := W+20; // Reduces the resize flickering when the text is changed to often
  SetControlHeightWidth;
  h := CalculateFormHeight + FDefaultBorderWidth*2;
  h := Round(h/10)*10;
  if H > Screen.Height-100 then
    H := Screen.Height-100;
  if H < (ClientHeight -20) then // Reduces the resize flickering when the text is changed to often
    ClientHeight := H
  else if H > ClientHeight then
    ClientHeight := H+5; // Reduces the resize flickering when the text is changed to often
end;

procedure TJvThreadBaseDialogForm.SetFormInfoText;
begin
  if (csDestroying in ComponentState) or not FormIsShown then
    Exit;
  if Assigned(IInfoTextControlCaption) and Assigned(DialogOptions) then
    if IInfoTextControlCaption.ControlGetCaption<>DialogOptions.InfoText then
    begin
      IInfoTextControlCaption.ControlSetCaption(DialogOptions.FInfoText);
      if Assigned(IInfoTextControlAutoSize) then
      begin
        IInfoTextControlAutoSize.ControlSetAutoSize(True);
//        IInfoTextControlAutoSize.ControlSetAutoSize(False);
      end;
      FInfoText.Left := FDefaultBorderWidth; // Some Components change the left position when activating autosize (TcxStaticText)
      FOrgInfoTextWidth := FInfoText.Width;
    end;
  FInfoTextPanel.Visible := DialogOptions.FInfoText <> '';
end;

procedure TJvThreadBaseDialogForm.UpdateFormContents;
begin
  if (csDestroying in ComponentState) or not FormIsShown then
    Exit;
  inherited UpdateFormContents;
  if Assigned(DialogOptions) then
  begin
    FTimeTextPanel.Visible := DialogOptions.ShowElapsedTime;
    FCancelBtn.Enabled := DialogOptions.EnableCancelButton;
    FCancelButtonPanel.Visible := DialogOptions.ShowCancelButton;
    if Assigned(ChangeThreadDialogOptions) then
      ChangeThreadDialogOptions(DialogOptions);

    SetFormInfoText;

    if Assigned(ITimeTextControlCaption) then
      ITimeTextControlCaption.ControlSetCaption (FormatDateTime('hh:nn:ss', Now - FStartTime));

  end;
  SetFormHeightWidth;
end;

function TJvThreadBaseDialogForm.GetDialogOptions: TJvThreadBaseDialogOptions;
begin
  Result := TJvThreadSimpleDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadBaseDialogForm.SetDialogOptions(const Value: TJvThreadBaseDialogOptions);
begin
  inherited DialogOptions := Value;
end;

procedure TJvThreadAnimateDialogOptions.Assign(Source: TPersistent);
begin
  if Source is TJvThreadAnimateDialogOptions then
  begin
    CommonAVI := TJvThreadAnimateDialogOptions(Source).CommonAVI;
    FileName := TJvThreadAnimateDialogOptions(Source).FileName;
    ResName := TJvThreadAnimateDialogOptions(Source).ResName;
  end;
  Inherited Assign(Source);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
