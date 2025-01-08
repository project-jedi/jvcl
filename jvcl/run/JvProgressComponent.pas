{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDlg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):
Zinvob
boerema

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

components  : TProgressForm
description : dialog components

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvProgressComponent;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Controls, Forms, StdCtrls, ComCtrls,
  SysUtils, Classes,
  JvComponentBase;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvProgressComponent = class(TJvComponent)
  private
    FForm: TForm;
    FProgressBar: TProgressBar;
    FLabel1: TLabel;
    FCaption: TCaption;
    FInfoLabel: TCaption;
    FOnShow: TNotifyEvent;
    FCancel: Boolean;
    FProgressMin: Integer;
    FProgressMax: Integer;
    FProgressStep: Integer;
    FProgressPosition: Integer;
    FException: Exception;
    FOnClose: TNotifyEvent;
    procedure SetCaption(ACaption: TCaption);
    procedure SetInfoLabel(ACaption: TCaption);
    procedure FormOnShow(Sender: TObject);
    procedure FormOnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormOnCancel(Sender: TObject);
    procedure SetProgressMax(const Value: Integer);
    procedure SetProgressMin(const Value: Integer);
    procedure SetProgressPosition(const Value: Integer);
    procedure SetProgressStep(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    procedure Hide;
    procedure ProgressStepIt;
    property Cancel: Boolean read FCancel;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property InfoLabel: TCaption read FInfoLabel write SetInfoLabel;
    property ProgressMin: Integer read FProgressMin write SetProgressMin default 0;
    property ProgressMax: Integer read FProgressMax write SetProgressMax default 100;
    property ProgressStep: Integer read FProgressStep write SetProgressStep default 1;
    property ProgressPosition: Integer read FProgressPosition write SetProgressPosition default 0;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
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
  JvConsts, JvResources, JvJCLUtils, JvComponent;

type
  TCMShowEvent = record
    Msg: Cardinal;
    {$IFDEF COMPILER16_UP}
	MsgFiller: TDWordFiller;
    {$ENDIF COMPILER16_UP}
    Unused: WPARAM;
    Instance: TJvProgressComponent;
    Result: LRESULT;
  end;

  TJvProgressForm = class(TJvForm)
  private
    procedure CMShowEvent(var Msg: TCMShowEvent); message CM_SHOWEVENT;
  end;

//=== { TJvProgressComponent } ===============================================

constructor TJvProgressComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProgressMin := 0;
  FProgressMax := 100;
  FProgressStep := 1;
  FProgressPosition := 0;
end;

destructor TJvProgressComponent.Destroy;
begin
  FForm.Free;
  inherited Destroy;
end;

procedure TJvProgressComponent.Hide;
begin
  FForm.Free;
  FForm := nil;
end;

procedure TJvProgressComponent.Execute;
begin
  if not Assigned(FForm) then
  begin
    // Use CreateNew in its BCB compatible version. And if we are
    // in design mode, the owner cannot be Self or the form would simply
    // not show up. Using Application as the owner solves this.
    if csDesigning in ComponentState then
      FForm := TJvProgressForm.CreateNew(Application, 1)
    else
      FForm := TJvProgressForm.CreateNew(Self, 1);
  end;
  try
    FForm.Caption := Caption;
    with FForm do
    begin
      ClientWidth := 307;
      ClientHeight := 98;
      BorderStyle := bsDialog;
      Position := poMainFormCenter;
      FormStyle := fsStayOnTop;
      Scaled := False;

      FProgressBar := TProgressBar.Create(FForm);
    end;
    with FProgressBar do
    begin
      Parent := FForm;
      if FProgressMin > Max then
      begin
        Max := FProgressMax;
        Min := FProgressMin;
      end
      else
      begin
        Min := FProgressMin;
        Max := FProgressMax;
      end;
      SetBounds(8, 38, 292, 18);
      if FProgressStep = 0 then
        FProgressStep := 1;
      Step := FProgressStep;
      Position := FProgressPosition;
    end;
    FLabel1 := TLabel.Create(FForm);
    with FLabel1 do
    begin
      Parent := FForm;
      Caption := InfoLabel;
      AutoSize := False;
      SetBounds(8, 8, 293, FForm.Canvas.TextHeight('Wq'));
    end;
    with TButton.Create(FForm) do
    begin
      Parent := FForm;
      Caption := RsButtonCancelCaption;
      SetBounds(116, 67, 75, 23);
      OnClick := FormOnCancel;
    end;
    FCancel := False;

    FForm.OnClose := FormOnClose;

    // Mantis 3430: In design mode, there is no main form, hence show
    // at the center, and in modal state to prevent the form from being
    // lost in the background.
    if csDesigning in ComponentState then
    begin
      FForm.Position := poScreenCenter;
      FForm.ShowModal;
    end
    else
    begin
      if Assigned(FOnShow) then
      begin
        FForm.OnShow := FormOnShow;
        FException := nil;
        FForm.ShowModal;
        if FException <> nil then
          raise FException;
      end
      else
        FForm.Show;
    end;
  finally
    // Force recreating the window every time it is shown at design time.
    if Assigned(FOnShow) or (csDesigning in ComponentState) then
      FreeAndNil(FForm);
  end;
end;

procedure TJvProgressComponent.FormOnShow(Sender: TObject);
begin
  PostMessage(FForm.Handle, CM_SHOWEVENT, 0, LPARAM(Self));
end;

procedure TJvProgressComponent.FormOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TJvProgressComponent.FormOnCancel(Sender: TObject);
begin
  FCancel := True;

  // Mantis 3430: In design mode, automatically hide the form upon cancellation
  if (csDesigning in ComponentState) and Assigned(FForm) then
    FForm.ModalResult := mrCancel;
end;

procedure TJvProgressForm.CMShowEvent(var Msg: TCMShowEvent);
begin
  Application.ProcessMessages;
  try
    try
      Msg.Instance.FOnShow(Self);
//      (Owner as TJvProgressComponent).FOnShow(Self);
    except
      on E: Exception do
        (Owner as TJvProgressComponent).FException := AcquireExceptionObject{$IFDEF RTL320_UP} as Exception{$ENDIF RTL320_UP};
    end;
  finally
    ModalResult := mrOk;
  end;
end;

procedure TJvProgressComponent.SetCaption(ACaption: TCaption);
begin
  FCaption := ACaption;
  if FForm <> nil then
    FForm.Caption := FCaption;
end;

procedure TJvProgressComponent.SetInfoLabel(ACaption: TCaption);
begin
  FInfoLabel := ACaption;
  if FForm <> nil then
    FLabel1.Caption := ACaption;
end;

procedure TJvProgressComponent.SetProgressMax(const Value: Integer);
begin
  FProgressMax := Value;
  if FForm <> nil then
    FProgressBar.Max := FProgressMax;
end;

procedure TJvProgressComponent.SetProgressMin(const Value: Integer);
begin
  FProgressMin := Value;
  if FForm <> nil then
    FProgressBar.Min := FProgressMin;
end;

procedure TJvProgressComponent.SetProgressPosition(const Value: Integer);
begin
  FProgressPosition := Value;
  if FForm <> nil then
    FProgressBar.Position := FProgressPosition;
end;

procedure TJvProgressComponent.SetProgressStep(const Value: Integer);
begin
  FProgressStep := Value;
  if FForm <> nil then
    FProgressBar.Step := FProgressStep;
end;

procedure TJvProgressComponent.ProgressStepIt;
begin
  if FForm <> nil then
  begin
    Inc(FProgressPosition, ProgressStep);
    if ProgressPosition > ProgressMax then
      FProgressPosition := ProgressMax
    else
    if ProgressPosition < ProgressMin then
      FProgressPosition := ProgressMin
    else
      FProgressBar.StepIt;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
