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
// $Id$

unit JvThreadDialog;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Forms, Buttons, StdCtrls,
  {$IFDEF MSWINDOWS}
  Windows, Controls, ComCtrls, ExtCtrls,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QWindows,
  {$ENDIF UNIX}
  JvTypes, JvComponentBase, JvThread, JvDynControlEngine;

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
  published
    property CancelButtonCaption: string read FCancelButtonCaption
      write SetCancelButtonCaption;
    property Caption: string read FCaption write SetCaption;
    property EnableCancelButton: Boolean read FEnableCancelButton write SetEnableCancelButton default True;
    property InfoText: string read FInfoText write SetInfoText;
    property InfoTextAlignment: TAlignment read FInfoTextAlignment write FInfoTextAlignment default taLeftJustify;
    property ShowCancelButton: Boolean read FShowCancelButton write SetShowCancelButton default True;
    property ShowElapsedTime: Boolean read FShowElapsedTime write SetShowElapsedTime default True;
  end;

  TJvThreadAnimateDialogOptions = class(TJvThreadBaseDialogOptions)
  private
    FCommonAVI: TCommonAVI;
    FFileName: string;
    FResName: string;
  published
    property CommonAVI: TCommonAVI read FCommonAVI write FCommonAVI;
    property FileName: string read FFileName write FFileName;
    property ResName: string read FResName write FResName;
  end;

  TJvThreadAnimateDialog = class(TJvCustomThreadDialog)
  private
    FChangeThreadDialogOptions: TJvChangeThreadDialogOptionsEvent;
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; override;
    function GetDialogOptions: TJvThreadAnimateDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadAnimateDialogOptions);
  public
    function CreateThreadDialogForm(ConnectedThread: TJvThread): TJvCustomThreadDialogForm; override;
  published
    property DialogOptions: TJvThreadAnimateDialogOptions read GetDialogOptions write SetDialogOptions;
    property OnPressCancel;
    property ChangeThreadDialogOptions: TJvChangeThreadDialogOptionsEvent read
        FChangeThreadDialogOptions write FChangeThreadDialogOptions;
  end;

  TJvThreadSimpleDialogOptions = class(TJvThreadBaseDialogOptions)
  private
    FShowProgressBar: Boolean;
    procedure SetShowProgressBar(const Value: Boolean);
  public
    constructor Create(AOwner: TJvCustomThreadDialog); override;
  published
    property ShowProgressBar: Boolean read FShowProgressBar write SetShowProgressBar default False;
  end;

  TJvThreadSimpleDialog = class(TJvCustomThreadDialog)
  private
    FChangeThreadDialogOptions: TJvChangeThreadDialogOptionsEvent;
    function GetDialogOptions: TJvThreadSimpleDialogOptions;
    procedure SetChangeThreadDialogOptions(const Value:
        TJvChangeThreadDialogOptionsEvent);
    procedure SetDialogOptions(Value: TJvThreadSimpleDialogOptions);
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; override;
  public
    function CreateThreadDialogForm(ConnectedThread: TJvThread): TJvCustomThreadDialogForm; override;
  published
    property DialogOptions: TJvThreadSimpleDialogOptions read GetDialogOptions write SetDialogOptions;
    property OnPressCancel;
    property ChangeThreadDialogOptions: TJvChangeThreadDialogOptionsEvent read
        FChangeThreadDialogOptions write SetChangeThreadDialogOptions;
  end;

  TJvDynControlEngineThreadDialogForm = class(TJvCustomThreadDialogForm)
  private
    FDefaultBorderWidth: Integer;
    function GetDynControlEngine: TJvDynControlEngine;
  protected
    procedure CreateTextPanel(AOwner: TComponent; AParent: TWinControl; var Panel:
        TWinControl; var Text: TControl; TextAlignment: TAlignment; const BaseName:
        string);
    property DefaultBorderWidth: Integer read FDefaultBorderWidth write
        FDefaultBorderWidth;
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine;
  end;

  TJvThreadSimpleDialogForm = class(TJvDynControlEngineThreadDialogForm)
  private
    FCancelBtn: TButton;
    FCancelButtonPanel: TWinControl;
    FChangeThreadDialogOptions: TJvChangeThreadDialogOptionsEvent;
    FCounter: Integer;
    FInfoText: TControl;
    FInfoTextPanel: TWinControl;
    FMainPanel: TWinControl;
    FOrgInfoTextWidth: Integer;
    FProgressbar: TWinControl;
    FProgressbarPanel: TWinControl;
    FStartTime: TDateTime;
    FTimeText: TControl;
    FTimeTextPanel: TWinControl;
    function GetDialogOptions: TJvThreadSimpleDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadSimpleDialogOptions);
    procedure SetFormInfoText;
  protected
    procedure CreateFormControls;
    procedure InitializeFormContents; override;
    procedure SetFormHeightWidth;
    procedure UpdateFormContents; override;
  public
    property DialogOptions: TJvThreadSimpleDialogOptions read GetDialogOptions write SetDialogOptions;
    property ChangeThreadDialogOptions: TJvChangeThreadDialogOptionsEvent read
        FChangeThreadDialogOptions write FChangeThreadDialogOptions;
  end;

  TJvThreadAnimateDialogForm = class(TJvDynControlEngineThreadDialogForm)
  private
    FAnimate: TAnimate;
    FAnimatePanel: TWinControl;
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
    function GetDialogOptions: TJvThreadAnimateDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadAnimateDialogOptions);
    //procedure SetFormInfoText;
  protected
    procedure CreateFormControls;
    procedure InitializeFormContents; override;
    procedure SetFormHeightWidth;
    procedure UpdateFormContents; override;
  public
    property DialogOptions: TJvThreadAnimateDialogOptions read GetDialogOptions write SetDialogOptions;
    property ChangeThreadDialogOptions: TJvChangeThreadDialogOptionsEvent read
        FChangeThreadDialogOptions write FChangeThreadDialogOptions;
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
  Dialogs, Graphics,
  JvResources, JvDynControlEngineIntf;

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
    ThreadDialogForm := TJvThreadSimpleDialogForm.CreateNewFormStyle(ConnectedThread,
      DialogOptions.FormStyle);
    ThreadDialogForm.DialogOptions := DialogOptions;
    ThreadDialogForm.OnPressCancel := OnPressCancel;
    ThreadDialogForm.ChangeThreadDialogOptions := ChangeThreadDialogOptions;
    ThreadDialogForm.CreateFormControls;
    Result := ThreadDialogForm;
  end
  else
    Result := nil;
end;

function TJvThreadSimpleDialog.GetDialogOptions: TJvThreadSimpleDialogOptions;
begin
  Result := TJvThreadSimpleDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadSimpleDialog.SetChangeThreadDialogOptions(const Value:
    TJvChangeThreadDialogOptionsEvent);
begin
  FChangeThreadDialogOptions := Value;
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
    ThreadDialogForm.CreateFormControls;
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
end;

procedure TJvThreadSimpleDialogOptions.SetShowProgressBar(const Value: Boolean);
begin
  FShowProgressBar := Value;
end;

procedure TJvThreadSimpleDialogForm.CreateFormControls;
var
  ITmpPanel: IJvDynControlPanel;
  ITmpAlign: IJvDynControlAlign;
begin
  FDefaultBorderWidth := 3;
  FMainPanel := DynControlEngine.CreatePanelControl(Self, Self,
    'MainPanel', '', alClient);
  if not Supports(FMainPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  ITmpPanel.ControlSetBorder(bvNone, bvNone, 0, bsNone, FDefaultBorderWidth);

  CreateTextPanel(Self, FMainPanel, FInfoTextPanel, FInfoText,
    DialogOptions.InfoTextAlignment, 'Info');
  CreateTextPanel(Self, FMainPanel, FTimeTextPanel, FTimeText, taCenter, 'Time');

  FProgressbarPanel := DynControlEngine.CreatePanelControl(Self,
    FMainPanel, 'ProgressbarPanel', '', alTop);
  if not Supports(FProgressbarPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  ITmpPanel.ControlSetBorder(bvNone, bvNone, 0, bsNone, FDefaultBorderWidth);
  FProgressbar := DynControlEngine.CreateProgressbarControl(Self, FProgressbarPanel,
    'Progressbar');
  FProgressbarPanel.Height := FProgressbar.Height + FDefaultBorderWidth*2;
  if Supports(FProgressbar, IJvDynControlAlign, ITmpAlign) then
    ITmpAlign.ControlSetAlign(alClient);

  FCancelButtonPanel := DynControlEngine.CreatePanelControl(Self,
    FMainPanel, 'ButtonPanel', '', alTop);
  FCancelBtn := DynControlEngine.CreateButton(Self, FCancelButtonPanel,
    'CancelBtn', DialogOptions.CancelButtonCaption, '', DefaultCancelBtnClick,
    True, True);
  FCancelBtn.Anchors := [akTop];
  FCancelBtn.Top := FDefaultBorderWidth;
  FCancelButtonPanel.Height := FCancelBtn.Height + FDefaultBorderWidth*2;

  BorderIcons := [];
  BorderStyle := bsDialog;
  Caption := ' ';
  ClientHeight := 88;
  ClientWidth := 268;
  FormStyle := DialogOptions.FormStyle;
  {$IFDEF COMPILER7_UP}
  Position := poOwnerFormCenter;
  {$ELSE}
  Position := poScreenCenter;
  {$ENDIF COMPILER7_UP}

  UpdateFormContents;
end;

function TJvThreadSimpleDialogForm.GetDialogOptions: TJvThreadSimpleDialogOptions;
begin
  Result := TJvThreadSimpleDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadSimpleDialogForm.InitializeFormContents;
begin
  inherited InitializeFormContents;
  SetFormHeightWidth;
  FStartTime := Now;
  FCounter   := 0;
end;

procedure TJvThreadSimpleDialogForm.SetDialogOptions(Value:
  TJvThreadSimpleDialogOptions);
begin
  inherited DialogOptions := Value;
end;

procedure TJvThreadSimpleDialogForm.SetFormHeightWidth;
var
  H, W: Integer;
begin
  if (csDestroying in ComponentState) then
    Exit;
  if FInfoTextPanel.Visible then
    W := FOrgInfoTextWidth + 20
  else
    W := 0;
  W := Round(W/10)*10;
  if W < 250 then
    W := 250;
  if W > Screen.Width-100 then
    W := Screen.Width-100;
  if (ClientWidth < W) or (ClientWidth > W+50)then // Reduces the resize flickering when the text is changed to often
    ClientWidth := W;
  FCancelBtn.Left := (FCancelButtonPanel.Width - FCancelBtn.Width) div 2;
  FInfoText.Width := FInfoTextPanel.Width-FDefaultBorderWidth*2;
  FInfoTextPanel.Height := FInfoText.Height+FDefaultBorderWidth*2;
  FTimeText.Width := FTimeTextPanel.Width-FDefaultBorderWidth*2;
  FTimeTextPanel.Height := FTimeText.Height+FDefaultBorderWidth*2;
  FProgressbarPanel.Width := FTimeTextPanel.Width-FDefaultBorderWidth*2;
  H := 0;
  if FInfoTextPanel.Visible then
    H := FInfoTextPanel.Height;
  if FTimeTextPanel.Visible then
    H := H + FTimeTextPanel.Height;
  if FProgressbarPanel.Visible then
    H := H + FProgressbarPanel.Height;
  if FCancelButtonPanel.Visible then
    H := H + FCancelButtonPanel.Height;
  H := H + FDefaultBorderWidth*2;
  if H > Screen.Height-100 then
    H := Screen.Height-100;
  if ClientHeight <> H then
    ClientHeight := H;
end;

procedure TJvThreadSimpleDialogForm.SetFormInfoText;
var
  ITmpControl: IJvDynControlCaption;
  ITmpAutoSize: IJvDynControlAutoSize;
begin
  if (csDestroying in ComponentState) then
    Exit;
  if Supports(FInfoText, IJvDynControlCaption, ITmpControl) then
    if ITmpControl.ControlGetCaption<>DialogOptions.FInfoText then
    begin
      ITmpControl.ControlSetCaption(DialogOptions.FInfoText);
      if Supports(FInfoText, IJvDynControlAutoSize, ITmpAutoSize) then
      begin
        ITmpAutoSize.ControlSetAutoSize(True);
        ITmpAutoSize.ControlSetAutoSize(False);
      end;
      FInfoText.Left := FDefaultBorderWidth; // Some Components change the left position when activating autosize (TcxStaticText)
      FOrgInfoTextWidth := FInfoText.Width;
    end;
  FInfoTextPanel.Visible := DialogOptions.FInfoText <> '';
end;

procedure TJvThreadSimpleDialogForm.UpdateFormContents;
var
  ITmpControl: IJvDynControlCaption;
  ITmpProgressbar : IJvDynControlProgressbar;
begin
  if (csDestroying in ComponentState) then
    Exit;
  inherited UpdateFormContents;
  FCounter := FCounter + 1;
  if Assigned(DialogOptions) then
  begin
    FTimeTextPanel.Visible := DialogOptions.ShowElapsedTime;
    FCancelBtn.Enabled := DialogOptions.EnableCancelButton;
    FCancelButtonPanel.Visible := DialogOptions.ShowCancelButton;
    FProgressbarPanel.Visible := DialogOptions.ShowProgressBar;
    if Assigned(ChangeThreadDialogOptions) then
      ChangeThreadDialogOptions(DialogOptions);

    SetFormInfoText;

    if Supports(FTimeText, IJvDynControlCaption, ITmpControl) then
      ITmpControl.ControlSetCaption (FormatDateTime('hh:nn:ss', Now - FStartTime));

    if Supports(FProgressbar, IJvDynControlProgressbar, ITmpProgressbar) then
      ITmpProgressbar.ControlSetPosition(((FCounter*10) mod 110));
    case FCounter mod 4 of
      0: Caption := DialogOptions.Caption + ' | ';
      1: Caption := DialogOptions.Caption + ' / ';
      2: Caption := DialogOptions.Caption + ' --';
    else
      Caption := DialogOptions.Caption + ' \ ';
    end;
  end;
  SetFormHeightWidth;
end;

procedure TJvThreadAnimateDialogForm.CreateFormControls;
var
  ITmpPanel: IJvDynControlPanel;
begin
  FDefaultBorderWidth:=3;
  FMainPanel := DynControlEngine.CreatePanelControl(Self, Self,
    'MainPanel', '', alClient);
  if not Supports(FMainPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  ITmpPanel.ControlSetBorder(bvNone, bvNone, 0, bsNone, FDefaultBorderWidth);

  CreateTextPanel(Self, FMainPanel, FInfoTextPanel, FInfoText,
    DialogOptions.InfoTextAlignment,  'Info');

  FAnimatePanel := DynControlEngine.CreatePanelControl(Self, FMainPanel,
    'AnimatePanel', '', alTop);
  if not Supports(FAnimatePanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  ITmpPanel.ControlSetBorder(bvNone, bvNone, 0, bsNone, FDefaultBorderWidth);

  FAnimate := TAnimate.Create(Self);
  FAnimate.Parent := FAnimatePanel;
  FAnimate.Top := FDefaultBorderWidth;
  FAnimate.Left := FDefaultBorderWidth;
  FAnimate.AutoSize := True;
  FAnimate.CommonAVI := TJvThreadAnimateDialogOptions(DialogOptions).CommonAVI;
  FAnimate.FileName := TJvThreadAnimateDialogOptions(DialogOptions).FileName;
  FAnimate.ResName := TJvThreadAnimateDialogOptions(DialogOptions).ResName;
  FAnimatePanel.Height := FAnimate.Height + FDefaultBorderWidth*2;

  CreateTextPanel(Self, FMainPanel, FTimeTextPanel, FTimeText, taCenter,  'Time');

  FCancelButtonPanel := DynControlEngine.CreatePanelControl(Self,
    FMainPanel, 'ButtonPanel', '', alTop);
  FCancelBtn := DynControlEngine.CreateButton(Self, FCancelButtonPanel,
    'CancelBtn', DialogOptions.CancelButtonCaption, '', DefaultCancelBtnClick,
    True, True);
  FCancelBtn.Anchors := [akTop];
  FCancelBtn.Top := FDefaultBorderWidth;
  FCancelButtonPanel.Height := FCancelBtn.Height + FDefaultBorderWidth*2;

  BorderIcons := [];
  BorderStyle := bsDialog;
  Caption := ' ';
  FormStyle := DialogOptions.FormStyle;
  {$IFDEF COMPILER7_UP}
  Position := poOwnerFormCenter;
  {$ELSE}
  Position := poScreenCenter;
  {$ENDIF COMPILER7_UP}
  UpdateFormContents;
end;

function TJvThreadAnimateDialogForm.GetDialogOptions: TJvThreadAnimateDialogOptions;
begin
  Result := TJvThreadAnimateDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadAnimateDialogForm.InitializeFormContents;
begin
  inherited InitializeFormContents;
  SetFormHeightWidth;
  FStartTime := Now;
  FAnimate.Active := True;
end;

procedure TJvThreadAnimateDialogForm.SetDialogOptions(Value:
  TJvThreadAnimateDialogOptions);
begin
  inherited DialogOptions := Value;
end;

procedure TJvThreadAnimateDialogForm.SetFormHeightWidth;
var
  H, W: Integer;
begin
  if (csDestroying in ComponentState) then
    Exit;
  H := 0;
  W := 200;

  if FInfoTextPanel.Visible then
    W := Max(FOrgInfoTextWidth + 80, W);
  if FAnimatePanel.Visible then
    W := Max(W, FAnimate.Width + 20);

  if ClientWidth <> W then
    ClientWidth := W;

  FCancelBtn.Left := (FCancelButtonPanel.Width - FCancelBtn.Width) div 2;
  FAnimate.Left   := (FAnimatePanel.Width - FAnimate.Width) div 2;
  FInfoText.Width := FInfoTextPanel.Width-FDefaultBorderWidth*2;
  FInfoTextPanel.Height := FInfoText.Height+FDefaultBorderWidth*2;
  FTimeText.Width := FTimeTextPanel.Width-FDefaultBorderWidth*2;
  FTimeTextPanel.Height := FTimeText.Height+FDefaultBorderWidth*2;

  if FInfoTextPanel.Visible then
  begin
    FInfoTextPanel.Top := h;
    H := H + FInfoTextPanel.Height;
  end;
  if FAnimatePanel.Visible then
  begin
    FAnimatePanel.Top := h;
    H := H + FAnimatePanel.Height;
  end;
  if FTimeTextPanel.Visible then
  begin
    FTimeTextPanel.Top := h;
    H := H + FTimeTextPanel.Height;
  end;
  if FCancelButtonPanel.Visible then
  begin
    FCancelButtonPanel.Top := h;
    H := H + FCancelButtonPanel.Height;
  end;
  H := H + 6;
  if ClientHeight <> H then
    ClientHeight := H;
end;

procedure TJvThreadAnimateDialogForm.UpdateFormContents;
var ITmpControl : IJvDynControlCaption;
begin
  if (csDestroying in ComponentState) then
    Exit;
  inherited UpdateFormContents;
  if Assigned(DialogOptions) then
  begin
    if Assigned(ChangeThreadDialogOptions) then
      ChangeThreadDialogOptions(DialogOptions);
    if Supports(FInfoText, IJvDynControlCaption, ITmpControl) then
      ITmpControl.ControlSetCaption(DialogOptions.FInfoText);
    if Supports(FTimeText, IJvDynControlCaption, ITmpControl) then
      ITmpControl.ControlSetCaption(FormatDateTime('hh:nn:ss', Now - FStartTime));
    Caption := DialogOptions.Caption;
    FInfoTextPanel.Visible := DialogOptions.InfoText <> '';
    FAnimatePanel.Visible := FileExists(FAnimate.FileName) or
      (FAnimate.CommonAVI <> aviNone) or (FAnimate.ResName <> '');
    FTimeTextPanel.Visible := DialogOptions.ShowElapsedTime;
    FCancelBtn.Enabled := DialogOptions.EnableCancelButton;
    FCancelButtonPanel.Visible := DialogOptions.ShowCancelButton;
  end;
  SetFormHeightWidth;
end;

procedure TJvDynControlEngineThreadDialogForm.CreateTextPanel(AOwner:
    TComponent; AParent: TWinControl; var Panel: TWinControl; var Text:
    TControl; TextAlignment: TAlignment; const BaseName: string);
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

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
