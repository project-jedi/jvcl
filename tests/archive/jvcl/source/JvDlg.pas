{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDlg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):
Zinvob

Last Modified: 2002-11-19

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

components  : TProgressForm
description : dialog components

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDlg;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Controls, Forms, StdCtrls, ComCtrls,
  JvComponent;

type
  TJvProgressForm = class(TJvComponent)
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
    procedure SetCaption(ACaption: TCaption);
    procedure SetInfoLabel(ACaption: TCaption);
    procedure FormOnShow(Sender: TObject);
    procedure FormOnCancel(Sender: TObject);
    procedure SetProgress(Index: Integer; AValue: Integer);
  public
    destructor Destroy; override;
    procedure Execute;
    procedure ProgressStepIt;
    property Cancel: Boolean read FCancel;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property InfoLabel: TCaption read FInfoLabel write SetInfoLabel;
    property ProgressMin: Integer Index 0 read FProgressMin write SetProgress;
    property ProgressMax: Integer Index 1 read FProgressMax write SetProgress;
    property ProgressStep: Integer Index 2 read FProgressStep write SetProgress;
    property ProgressPosition: Integer Index 3 read FProgressPosition write SetProgress;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

implementation

uses
  JvCtlConst;

{$IFNDEF BCB3}

function ChangeTopException(E: TObject): TObject;
type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    //ExceptionRecord: PExceptionRecord;
  end;
begin
  { CBuilder 3 Warning !}
  { if linker error occured with message "unresolved external 'System::RaiseList'" try
    comment this function implementation, compile,
    then uncomment and compile again. }
  {$IFDEF COMPLIB_VCL}
  if RaiseList <> nil then
  begin
    Result := PRaiseFrame(RaiseList)^.ExceptObject;
    PRaiseFrame(RaiseList)^.ExceptObject := E
  end
  else
    Result := nil;
  {$ENDIF COMPLIB_VCL}
  {$IFDEF LINUX}
  // XXX: changing exception in stack frame is not supported on Kylix
  Writeln('ChangeTopException');
  Result := E;
  {$ENDIF LINUX}
end;
{$ENDIF BCB3}

{##################### From JvUtils unit #####################}

type
  TJvProgressFormForm = class(TForm)
  private
    procedure WMUser1(var Msg: TMessage); message WM_USER + 1;
  end;

procedure TJvProgressForm.Execute;
begin
  {$IFDEF BCB}
  if not Assigned(FForm) then
    FForm := TJvProgressFormForm.CreateNew(Owner, 1);
  {$ELSE}
  if not Assigned(FForm) then
    FForm := TJvProgressFormForm.CreateNew(Owner);
  {$ENDIF}
  try
    FForm.Caption := Caption;
    with FForm do
    begin
      ClientWidth := 307;
      ClientHeight := 98;
      BorderStyle := bsDialog;
      Position := poScreenCenter;
      FProgressBar := TProgressBar.Create(FForm);
    end;
    with FProgressBar do
    begin
      Parent := FForm;
      Min := FProgressMin;
      Max := FProgressMax;
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
      Caption := '';
      AutoSize := False;
      SetBounds(8, 8, 293, 13);
    end;
    with TButton.Create(FForm) do
    begin
      Parent := FForm;
      Caption := SCancel;
      SetBounds(116, 67, 75, 23);
      OnClick := FormOnCancel;
    end;
    FCancel := False;
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
  finally
    if Assigned(FOnShow) then
      FreeAndNil(FForm);
  end;
end;

procedure TJvProgressForm.FormOnShow(Sender: TObject);
begin
  PostMessage(FForm.Handle, WM_USER + 1, 0, 0);
end;

procedure TJvProgressForm.FormOnCancel(Sender: TObject);
begin
  FCancel := True;
end;

procedure TJvProgressFormForm.WMUser1(var Msg: TMessage);
begin
  Application.ProcessMessages;
  try
    try
      (Owner as TJvProgressForm).FOnShow(Self);
    except
      on E: Exception do
      begin
        {$IFNDEF BCB3}
        (Owner as TJvProgressForm).FException := E;
        ChangeTopException(nil);
        {$ENDIF BCB3}
        {$IFDEF BCB3}
        (Owner as TJvProgressForm).FException := Exception.Create(E.Message);
        {$ENDIF BCB3}
      end;
    end;
  finally
    ModalResult := mrOk;
  end;
end;

procedure TJvProgressForm.SetCaption(ACaption: TCaption);
begin
  FCaption := ACaption;
  if FForm <> nil then
    FForm.Caption := FCaption;
end;

procedure TJvProgressForm.SetInfoLabel(ACaption: TCaption);
begin
  FInfoLabel := ACaption;
  if FForm <> nil then
    FLabel1.Caption := ACaption;
end;

procedure TJvProgressForm.SetProgress(Index: Integer; AValue: Integer);
begin
  case Index of
    0:
      FProgressMin := AValue;
    1:
      FProgressMax := AValue;
    2:
      FProgressStep := AValue;
    3:
      FProgressPosition := AValue;
  end;
  if FForm <> nil then
  begin
    FProgressBar.Min := FProgressMin;
    FProgressBar.Max := FProgressMax;
    FProgressBar.Step := FProgressStep;
    FProgressBar.Position := FProgressPosition;
  end;
end;

procedure TJvProgressForm.ProgressStepIt;
begin
  if FForm <> nil then
    FProgressBar.StepIt;
end;

destructor TJvProgressForm.Destroy;
begin
  FForm.Free;
  inherited Destroy;
end;

end.

