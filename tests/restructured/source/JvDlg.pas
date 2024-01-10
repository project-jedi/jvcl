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

Last Modified: 2002-07-04

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
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Grids, StdCtrls, ComCtrls;

type

  TJvProgressForm = class(TComponent)
  private
    Form : TForm;
    FProgressBar : TProgressBar;
    Label1 : TLabel;

    FCaption : TCaption;
    FInfoLabel : TCaption;
    FOnShow : TNotifyEvent;
    FCancel : boolean;
    FProgressMin,
    FProgressMax,
    FProgressStep,
    FProgressPosition : integer;
    EE : Exception;

    procedure SetCaption(ACaption : TCaption);
    procedure SetInfoLabel(ACaption : TCaption);
    procedure FormOnShow(Sender : TObject);
    procedure FormOnCancel(Sender : TObject);
    procedure SetProgress(index : integer; AValue : integer);
  public
    procedure Execute;
    procedure ProgressStepIt;
    property Cancel : boolean read FCancel;
  published
    property Caption : TCaption read FCaption write SetCaption;
    property InfoLabel : TCaption read FInfoLabel write SetInfoLabel;
    property ProgressMin : integer index 0 read FProgressMin write SetProgress;
    property ProgressMax : integer index 1 read FProgressMax write SetProgress;
    property ProgressStep : integer index 2 read FProgressStep write SetProgress;
    property ProgressPosition : integer index 3 read FProgressPosition write SetProgress;
    property OnShow : TNotifyEvent read FOnShow write FOnShow;
  end;

implementation

uses JvCtlConst;

{$IFNDEF BCB3}
function ChangeTopException(E : TObject): TObject;
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
  { if linker error occurred with message "unresolved external 'System::RaiseList'" try
    comment this function implementation, compile,
    then uncomment and compile again. }
{$IFDEF COMPLIB_VCL}
  if RaiseList <> nil then
  begin
    Result := PRaiseFrame(RaiseList)^.ExceptObject;
    PRaiseFrame(RaiseList)^.ExceptObject := E
  end else
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
    procedure WMUser1(var Message : TMessage); message wm_User+1;
  end;

procedure TJvProgressForm.Execute;
begin
 {$IFDEF BCB}
  Form := TJvProgressFormForm.CreateNew(Self, 1);
 {$ELSE}
  Form := TJvProgressFormForm.CreateNew(Self);
 {$ENDIF BCB}
  try
    Form.Caption := Caption;
    with Form do
      begin
        ClientWidth := 307;
        ClientHeight := 98;
        BorderStyle := bsDialog;
        Position := poScreenCenter;
        FProgressBar := TProgressBar.Create(Form);
      end;
    with FProgressBar do begin
      Parent := Form;
      SetBounds(8, 38, 292, 18);
      Step := 1;
    end;
    Label1 := TLabel.Create(Form);
    with Label1 do begin
      Parent := Form;
      Caption := '';
      AutoSize := false;
      SetBounds(8, 8, 293, 13);
    end;
    with TButton.Create(Form) do begin
      Parent := Form;
      Caption := SCancel;
      SetBounds(116, 67, 75, 23);
      OnClick := FormOnCancel;
    end;
    Form.OnShow := FormOnShow;
    FCancel := false;
    if Assigned(FOnShow) then
    begin
      EE := nil;
      Form.ShowModal;
      if EE <> nil then raise EE;
    end;
  finally
    Form.Free;
    Form := nil;
  end;
end;

procedure TJvProgressForm.FormOnShow(Sender : TObject);
begin
  PostMessage(Form.Handle, wm_User + 1, 0, 0);
end;

procedure TJvProgressForm.FormOnCancel(Sender : TObject);
begin
  FCancel := true;
end;

procedure TJvProgressFormForm.WMUser1(var Message : TMessage);
begin
  Application.ProcessMessages;
  try
    try
      (Owner as TJvProgressForm).FOnShow(Self);
    except
      on E : Exception do
      begin
       {$IFNDEF BCB3}
        (Owner as TJvProgressForm).EE := E;
        ChangeTopException(nil);
       {$ENDIF BCB3}
       {$IFDEF BCB3}
        (Owner as TJvProgressForm).EE := Exception.Create(E.Message);
       {$ENDIF BCB3}
      end;
    end;
  finally
    ModalResult := mrOk;
  end;
end;

procedure TJvProgressForm.SetCaption(ACaption : TCaption);
begin
  FCaption := ACaption;
  if Form <> nil then Form.Caption := FCaption;
end;

procedure TJvProgressForm.SetInfoLabel(ACaption : TCaption);
begin
  FInfoLabel := ACaption;
  if Form <> nil then Label1.Caption := ACaption;
end;

procedure TJvProgressForm.SetProgress(index : integer; AValue : integer);
begin
  case index of
    0 : FProgressMin := AValue;
    1 : FProgressMax := AValue;
    2 : FProgressStep := AValue;
    3 : FProgressPosition := AValue;
  end;
  if Form <> nil then
  begin
    FProgressBar.Min  := FProgressMin ;
    FProgressBar.Max  := FProgressMax ;
    FProgressBar.Step := FProgressStep;
    FProgressBar.Position := FProgressPosition;
  end;
end;

procedure TJvProgressForm.ProgressStepIt;
begin
  if Form <> nil then
    FProgressBar.StepIt;
end;

end.


