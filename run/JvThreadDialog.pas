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
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Forms, Buttons, StdCtrls,
  {$IFDEF MSWINDOWS}
  Windows, Controls, ComCtrls, ExtCtrls,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QWindows, QControls, QExtCtrls,
  {$ENDIF UNIX}
  JvTypes, JvComponentBase, JvThread, JvdynControlEngine;

type

  TJvThreadBaseDialogOptions = class(TJvCustomThreadDialogOptions)
  private
    FCancelButtonCaption: string;
    FCaption: string;
    FEnableCancelButton: boolean;
    FInfoText: string;
    FInfoTextAlignment: TAlignment;
    FShowCancelButton: boolean;
    FShowElapsedTime: boolean;
  protected
    procedure SetCancelButtonCaption(Value: string);
    procedure SetCaption(Value: string);
    procedure SetEnableCancelButton(Value: boolean);
    procedure SetInfoText(Value: string);
    procedure SetShowCancelButton(Value: boolean);
    procedure SetShowElapsedTime(Value: boolean);
  public
    constructor Create(AOwner: TJvCustomThreadDialog); override;
  published
    property CancelButtonCaption: string Read FCancelButtonCaption
      Write SetCancelButtonCaption;
    property Caption: string Read FCaption Write SetCaption;
    property EnableCancelButton: boolean Read FEnableCancelButton
      Write SetEnableCancelButton default True;
    property InfoText: string Read FInfoText Write SetInfoText;
    property InfoTextAlignment: TAlignment Read FInfoTextAlignment
      Write FInfoTextAlignment default taLeftJustify;
    property ShowCancelButton: boolean Read FShowCancelButton
      Write SetShowCancelButton default True;
    property ShowElapsedTime: boolean Read FShowElapsedTime
      Write SetShowElapsedTime default True;
  end;

  TJvThreadAnimateDialogOptions = class(TJvThreadBaseDialogOptions)
  private
    FCommonAVI: TCommonAVI;
    FFileName: string;
  published
    property CommonAVI: TCommonAVI Read FCommonAVI Write FCommonAVI;
    property FileName: string Read FFileName Write FFileName;
  end;

  TJvThreadAnimateDialog = class(TJvCustomThreadDialog)
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; override;
    function GetDialogOptions: TJvThreadAnimateDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadAnimateDialogOptions);
  public
    function CreateThreadDialogForm(ConnectedThread: TJvThread):
      TJvCustomThreadDialogForm; override;
  published
    property DialogOptions: TJvThreadAnimateDialogOptions
      Read GetDialogOptions Write SetDialogOptions;
  end;

  TJvThreadSimpleDialogOptions = class(TJvThreadBaseDialogOptions)
  private
    FShowProgressBar: integer;
    procedure SetShowProgressBar(Value: integer);
  protected
    property ShowProgressBar: integer Read FShowProgressBar Write SetShowProgressBar;
  end;

  TJvThreadSimpleDialog = class(TJvCustomThreadDialog)
  private
    function GetDialogOptions: TJvThreadSimpleDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadSimpleDialogOptions);
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; override;
  public
    function CreateThreadDialogForm(ConnectedThread: TJvThread):
      TJvCustomThreadDialogForm; override;
  published
    property DialogOptions: TJvThreadSimpleDialogOptions
      Read GetDialogOptions Write SetDialogOptions;
  end;

  TJvDynControlEngineThreadDialogForm = class(TJvCustomThreadDialogForm)
  private
    function GetDynControlEngine: TJvDynControlEngine;
  protected
    property DynControlEngine: TJvDynControlEngine Read GetDynControlEngine;
  end;

  TJvThreadSimpleDialogForm = class(TJvDynControlEngineThreadDialogForm)
  private
    FCancelBtn: TButton;
    FCancelButtonPanel: TWinControl;
    FCounter: integer;
    FInfoText: TControl;
    FInfoTextPanel: TWinControl;
    FMainPanel: TWinControl;
    FStartTime: TDateTime;
    FTimeText: TControl;
    FTimeTextPanel: TWinControl;
    function GetDialogOptions: TJvThreadSimpleDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadSimpleDialogOptions);
  protected
    procedure CreateFormControls;
    procedure CreateTextPanel(AOwner: TComponent; AParent: TWinControl;
      var Panel: TWincontrol; var Text: TControl; TextAlignment: TAlignment;
      TextAutoSize: boolean; const BaseName: string);
    procedure InitializeFormContents; override;
    procedure SetFormData;
    procedure SetFormHeightWidth;
    procedure UpdateFormContents; override;
  public
    property DialogOptions: TJvThreadSimpleDialogOptions
      Read GetDialogOptions Write SetDialogOptions;
  end;

  TJvThreadAnimateDialogForm = class(TJvDynControlEngineThreadDialogForm)
  private
    FAnimate: TAnimate;
    FAnimatePanel: TWinControl;
    FCancelBtn: TButton;
    FCancelButtonPanel: TWinControl;
    FInfoText: TControl;
    FInfoTextPanel: TWinControl;
    FMainPanel: TWinControl;
    FStartTime: TDateTime;
    FTimeText: TControl;
    FTimeTextPanel: TWinControl;
    function GetDialogOptions: TJvThreadAnimateDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadAnimateDialogOptions);
  protected
    procedure CreateFormControls;
    procedure CreateTextPanel(AOwner: TComponent; AParent: TWinControl;
      var Panel: TWincontrol; var Text: TControl; TextAlignment: TAlignment;
      TextAutoSize: boolean; const BaseName: string);
    procedure InitializeFormContents; override;
    procedure SetFormData;
    procedure SetFormHeightWidth;
    procedure UpdateFormContents; override;
  public
    property DialogOptions: TJvThreadAnimateDialogOptions
      Read GetDialogOptions Write SetDialogOptions;
  end;

function Max(a, b: integer): integer;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Dialogs,
  JvResources, Graphics, JvDynControlEngineIntf;

function Max(a, b: integer): integer;
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
  FEnableCancelButton  := True;
  FShowCancelButton    := True;
  FShowElapsedTime     := True;
  FCancelButtonCaption := RsButtonCancelCaption;
  FInfoTextAlignment   := taLeftJustify;
end;

procedure TJvThreadBaseDialogOptions.SetCancelButtonCaption(Value: string);
begin
  FCancelButtonCaption := Value;
end;

procedure TJvThreadBaseDialogOptions.SetCaption(Value: string);
begin
  FCaption := Value;
end;

procedure TJvThreadBaseDialogOptions.SetEnableCancelButton(Value: boolean);
begin
  FEnableCancelButton := Value;
end;

procedure TJvThreadBaseDialogOptions.SetInfoText(Value: string);
begin
  FInfoText := Value;
end;

procedure TJvThreadBaseDialogOptions.SetShowCancelButton(Value: boolean);
begin
  FShowCancelButton := Value;
end;

procedure TJvThreadBaseDialogOptions.SetShowElapsedTime(Value: boolean);
begin
  FShowElapsedTime := Value;
end;

//=== { TJvThreadSimpleDialog } ==============================================

function TJvThreadSimpleDialog.CreateDialogOptions: TJvCustomThreadDialogOptions;
begin
  Result := TJvThreadSimpleDialogOptions.Create(Self);
end;

function TJvThreadSimpleDialog.CreateThreadDialogForm(ConnectedThread: TJvThread):
TJvCustomThreadDialogForm;
var
  ThreadDialogForm: TJvThreadSimpleDialogForm;
begin
  if DialogOptions.ShowDialog then
  begin
    ThreadDialogForm := TJvThreadSimpleDialogForm.CreateNewFormStyle(ConnectedThread,
      DialogOptions.FormStyle);
    ThreadDialogForm.DialogOptions := DialogOptions;
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

procedure TJvThreadSimpleDialogOptions.SetShowProgressBar(Value: integer);
begin
  FShowProgressBar := Value;
end;

procedure TJvThreadSimpleDialogForm.CreateTextPanel(AOwner: TComponent;
  AParent: TWinControl; var Panel: TWincontrol; var Text: TControl;
  TextAlignment: TAlignment; TextAutoSize: boolean; const BaseName: string);
var
  ITmpPanel:     IJvDynControlPanel;
  ITmpAutoSize:  IJvDynControlAutoSize;
  ITmpAlignment: IJvDynControlAlignment;
begin
  Panel := DynControlEngine.CreatePanelControl(AOwner, AParent,
    BaseName + 'Panel', '', alTop);
  if not Supports(Panel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 3);
  Text      := DynControlEngine.CreateLabelControl(AOwner,
    Panel, BaseName + 'StaticText', '', nil);
  Text.Top  := 0;
  Text.Left := 0;
  if Supports(Text, IJvDynControlAutoSize, ITmpAutoSize) then
    ITmpAutoSize.ControlSetAutoSize(TextAutoSize);
  if Supports(Text, IJvDynControlAlignment, ITmpAlignment) then
    ITmpAlignment.ControlSetAlignment(TextAlignment);
  with Text do
  begin
    Top    := 1;
    Left   := 1;
    Height := 13;
    Panel.Height := Height + 6;
  end;
end;

procedure TJvThreadSimpleDialogForm.CreateFormControls;
var
  ITmpPanel: IJvDynControlPanel;
begin
  FMainPanel := DynControlEngine.CreatePanelControl(Self, Self,
    'MainPanel', '', alClient);
  if not Supports(FMainPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 5);

  CreateTextPanel(Self, FMainPanel, FInfoTextPanel, FInfoText,
    DialogOptions.InfoTextAlignment, True, 'Info');
  CreateTextPanel(Self, FMainPanel, FTimeTextPanel, FTimeText, taCenter, False, 'Time');

  FCancelButtonPanel := DynControlEngine.CreatePanelControl(Self,
    FMainPanel, 'ButtonPanel', '', alTop);
  FCancelBtn := DynControlEngine.CreateButton(Self, FCancelButtonPanel,
    'CancelBtn', RsButtonCancelCaption, '', DefaultCancelBtnClick,
    True, True);
  with FCancelBtn do
  begin
    Anchors := [akTop];
    Top     := 2;
    FCancelButtonPanel.Height := FCancelBtn.Height + 3;
  end;

  BorderIcons  := [];
  BorderStyle  := bsDialog;
  Caption      := ' ';
  ClientHeight := 88;
  ClientWidth  := 268;
  FormStyle    := fsStayOnTop;
  {$IFDEF COMPILER7_UP}
  Position     := poOwnerFormCenter;
  {$ELSE}
  Position     := poScreenCenter;
  {$ENDIF COMPILER7_UP}

  SetFormData;
end;


procedure TJvThreadSimpleDialogForm.InitializeFormContents;
begin
  inherited InitializeFormContents;
  FStartTime := Now;
  FCounter   := 0;
end;

procedure TJvThreadSimpleDialogForm.SetFormData;
var
  ITmpControl: IJvDynControl;
begin
  if Assigned(DialogOptions) then
  begin
    if Supports(FInfoText, IJvDynControl, ITmpControl) then
      ITmpControl.ControlSetCaption(DialogOptions.FInfoText);
    Caption := DialogOptions.Caption;
    FTimeTextPanel.Visible := DialogOptions.ShowElapsedTime;
    FCancelBtn.Enabled := DialogOptions.EnableCancelButton;
    FCancelButtonPanel.Visible := DialogOptions.ShowCancelButton;
    SetFormHeightWidth;
  end;
end;

procedure TJvThreadSimpleDialogForm.SetFormHeightWidth;
var
  H, W: integer;
begin
  W := FInfoText.Width + 80;
  if W < 200 then
    W := 200;
  ClientWidth := W;
  FCancelBtn.Left := (FCancelButtonPanel.Width - FCancelBtn.Width) div 2;
  FInfoText.Width := FInfoTextPanel.Width - 6;
  FInfoTextPanel.Height := FInfoText.Height + 6;
  FTimeText.Width := FTimeTextPanel.Width - 6;
  H := FInfoTextPanel.Height;
  if FTimeTextPanel.Visible then
    H := H + FTimeTextPanel.Height;
  if FCancelButtonPanel.Visible then
    H := H + FCancelButtonPanel.Height;
  H := H + 10;
  if ClientHeight <> H then
    ClientHeight := H;
end;

procedure TJvThreadSimpleDialogForm.UpdateFormContents;
var
  ITmpControl: IJvDynControl;
begin
  inherited UpdateFormContents;
  FCounter := FCounter + 1;
  if Supports(FTimeText, IJvDynControl, ITmpControl) then
    ITmpControl.ControlSetCaption(FormatDateTime('hh:nn:ss', Now - FStartTime));
  case FCounter mod 4 of
    0: Caption := DialogOptions.Caption + ' | ';
    1: Caption := DialogOptions.Caption + ' / ';
    2: Caption := DialogOptions.Caption + ' --';
    3: Caption := DialogOptions.Caption + ' \ ';
  end;
end;

function TJvThreadSimpleDialogForm.GetDialogOptions: TJvThreadSimpleDialogOptions;
begin
  Result := TJvThreadSimpleDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadSimpleDialogForm.SetDialogOptions(Value:
  TJvThreadSimpleDialogOptions);
begin
  inherited DialogOptions := Value;
end;

procedure TJvThreadAnimateDialogForm.CreateFormControls;
var
  ITmpPanel: IJvDynControlPanel;
begin
  FMainPanel := DynControlEngine.CreatePanelControl(Self, Self,
    'MainPanel', '', alClient);
  if not Supports(FMainPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 5);

  CreateTextPanel(Self, FMainPanel, FInfoTextPanel, FInfoText,
    DialogOptions.InfoTextAlignment, True, 'Info');

  FAnimatePanel := DynControlEngine.CreatePanelControl(Self, FMainPanel,
    'AnimatePanel', '', alTop);
  if not Supports(FAnimatePanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 3);

  FAnimate := TAnimate.Create(Self);
  with FAnimate do
  begin
    Parent   := FAnimatePanel;
    Top      := 0;
    Left     := 0;
    AutoSize := True;
    CommonAVI := TJvThreadAnimateDialogOptions(DialogOptions).CommonAVI;
    FileName := TJvThreadAnimateDialogOptions(DialogOptions).FileName;
    FAnimatePanel.Height := Height + 6;
  end;

  CreateTextPanel(Self, FMainPanel, FTimeTextPanel, FTimeText, taCenter, False, 'Time');

  FCancelButtonPanel := DynControlEngine.CreatePanelControl(Self,
    FMainPanel, 'ButtonPanel', '', alTop);
  FCancelBtn := DynControlEngine.CreateButton(Self, FCancelButtonPanel,
    'CancelBtn', RsButtonCancelCaption, '', DefaultCancelBtnClick,
    True, True);
  with FCancelBtn do
  begin
    Anchors := [akTop];
    Top     := 2;
    FCancelButtonPanel.Height := FCancelBtn.Height + 3;
  end;



  BorderIcons  := [];
  BorderStyle  := bsDialog;
  Caption      := ' ';
  ClientHeight := 88;
  ClientWidth  := 268;
  FormStyle    := fsStayOnTop;
  {$IFDEF COMPILER7_UP}
  Position     := poOwnerFormCenter;
  {$ELSE}
  Position     := poScreenCenter;
  {$ENDIF COMPILER7_UP}



  SetFormData;
end;

procedure TJvThreadAnimateDialogForm.CreateTextPanel(AOwner: TComponent;
  AParent: TWinControl; var Panel: TWincontrol; var Text: TControl;
  TextAlignment: TAlignment; TextAutoSize: boolean; const BaseName: string);
var
  ITmpPanel:     IJvDynControlPanel;
  ITmpAutoSize:  IJvDynControlAutoSize;
  ITmpAlignment: IJvDynControlAlignment;
begin
  Panel := DynControlEngine.CreatePanelControl(AOwner, AParent,
    BaseName + 'Panel', '', alTop);
  if not Supports(Panel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 3);
  Text      := DynControlEngine.CreateLabelControl(AOwner,
    Panel, BaseName + 'StaticText', '', nil);
  Text.Top  := 0;
  Text.Left := 0;
  if Supports(Text, IJvDynControlAutoSize, ITmpAutoSize) then
    ITmpAutoSize.ControlSetAutoSize(TextAutoSize);
  if Supports(Text, IJvDynControlAlignment, ITmpAlignment) then
    ITmpAlignment.ControlSetAlignment(TextAlignment);
  with Text do
  begin
    Top    := 1;
    Left   := 1;
    Height := 13;
    Panel.Height := Height + 6;
  end;
end;

procedure TJvThreadAnimateDialogForm.InitializeFormContents;
begin
  inherited InitializeFormContents;
  FStartTime      := Now;
  FAnimate.Active := True;
end;

procedure TJvThreadAnimateDialogForm.SetFormData;
var
  ITmpControl: IJvDynControl;
begin
  if Assigned(DialogOptions) then
  begin
    if Supports(FInfoText, IJvDynControl, ITmpControl) then
      ITmpControl.ControlSetCaption(DialogOptions.FInfoText);
    if Supports(FTimeText, IJvDynControl, ITmpControl) then
      ITmpControl.ControlSetCaption(FormatDateTime('hh:nn:ss', 0));
    Caption := DialogOptions.Caption;
    FInfoTextPanel.Visible := DialogOptions.InfoText <> '';
    FAnimatePanel.Visible := FileExists(FAnimate.FileName) or
      (FAnimate.CommonAVI <> aviNone);
    FTimeTextPanel.Visible := DialogOptions.ShowElapsedTime;
    FCancelBtn.Enabled := DialogOptions.EnableCancelButton;
    FCancelButtonPanel.Visible := DialogOptions.ShowCancelButton;
    SetFormHeightWidth;
  end;
end;

procedure TJvThreadAnimateDialogForm.SetFormHeightWidth;
var
  H, W: integer;
begin
  H := 0;
  W := 200;

  if FInfoTextPanel.Visible then
  begin
    W := Max(FInfoText.Width + 80, W);
    FInfoTextPanel.Height := FInfoText.Height + 6;
    FInfoTextPanel.Top := h;
    H := H + FInfoTextPanel.Height;
  end;
  if FAnimatePanel.Visible then
  begin
    W := Max(W, FAnimate.Width + 20);
    FAnimatePanel.Top := h;
    H := H + FAnimatePanel.Height;
  end;
  FCancelBtn.Left := (W - FCancelBtn.Width) div 2;
  FAnimate.Left   := (W - FAnimate.Width) div 2;
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
  if ClientWidth <> W then
  begin
    ClientWidth     := W;
    FTimeTextPanel.Width := FMainPanel.Width;
    FInfoTextPanel.Width := FMainPanel.Width;
    FTimeText.Width := FTimeTextPanel.Width - 6;
    FInfoText.Width := FInfoTextPanel.Width - 6;
  end;
  H := H + 6;
  if ClientHeight <> H then
    ClientHeight := H;
end;

procedure TJvThreadAnimateDialogForm.UpdateFormContents;
var
  ITmpControl: IJvDynControl;
begin
  inherited UpdateFormContents;
  if Supports(FTimeText, IJvDynControl, ITmpControl) then
    ITmpControl.ControlSetCaption(FormatDateTime('hh:nn:ss', Now - FStartTime));
end;

function TJvThreadAnimateDialogForm.GetDialogOptions: TJvThreadAnimateDialogOptions;
begin
  Result := TJvThreadAnimateDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadAnimateDialogForm.SetDialogOptions(Value:
  TJvThreadAnimateDialogOptions);
begin
  inherited DialogOptions := Value;
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

