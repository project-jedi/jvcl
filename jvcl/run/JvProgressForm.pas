{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProgressComponent.PAS, released on 2003-03-31.

The Initial Developer of the Original Code is Peter Thörnqvist.
Portions created by Peter Thörnqvist are Copyright (c) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

Description:
- This form is used by JvProgressDialog.pas
-----------------------------------------------------------------------------}
// $Id$

unit JvProgressForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  Windows, Graphics, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, ActnList,
  JvComponent;

type
  TJvPrivateProgressUpdate = procedure(Sender: TObject;
    var AMin, AMax, APosition, AInterval: Integer;
    var ACaption, ALabel: string; AnImage: TPicture;
    var AContinue: Boolean) of object;

  TfrmProgress = class(TJvForm)
    pbProgress: TProgressBar;
    imProgress: TImage;
    Label1: TLabel;
    btnCancel: TButton;
    tmProgress: TTimer;
    ActionList1: TActionList;
    Action1: TAction;
    procedure tmProgressTimer(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
  private
    FOnProgress: TJvPrivateProgressUpdate;
    FOnCancel: TNotifyEvent;
    FCancelled, FCanClose: Boolean;
    function DoProgress: Boolean;
    procedure DoCancel;
    procedure AdjustComponents;
    procedure RemoveCaption;
    procedure AddCaption;
  public
    class function Execute(Frm: TfrmProgress; const ACaption, ALabel: string;
      AImage: TPicture = nil; ATransparent: Boolean = False;
      AMin: Integer = 0; AMax: Integer = 100; APosition: Integer = 0;
      AInterval: Integer = 200; ShowCancel: Boolean = False; Smooth: Boolean = False;
      AOnProgress: TJvPrivateProgressUpdate = nil;
      AOnCancel: TNotifyEvent = nil): Boolean;
    function ShowModal: Integer; override;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF VCL}
  Consts;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QConsts;
  {$ENDIF VisualCLX}

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

class function TfrmProgress.Execute(Frm: TfrmProgress; const ACaption, ALabel: string;
  AImage: TPicture; ATransparent: Boolean; AMin, AMax, APosition, AInterval: Integer;
  ShowCancel, Smooth: Boolean; AOnProgress: TJvPrivateProgressUpdate; AOnCancel: TNotifyEvent): Boolean;
var
  DoModal: Boolean;
begin
  if Frm = nil then
  begin
    Frm := Self.Create(Application);
    DoModal := True;
  end
  else
    DoModal := False;
  try
    with Frm do
    begin
      Caption := ACaption;
      Label1.Caption := ALabel;
      pbProgress.Min := AMin;
      pbProgress.Max := AMax;
      pbProgress.Position := APosition;
      pbProgress.Smooth := Smooth;
      FOnProgress := AOnProgress;
      imProgress.Picture := AImage;
      imProgress.Transparent := ATransparent;
      tmProgress.Interval := AInterval;
      tmProgress.Enabled := AInterval > 0;
      btnCancel.Visible := ShowCancel;
      FCanClose := ShowCancel;
      btnCancel.Caption := SCancelButton;
      FOnCancel := AOnCancel;
      AdjustComponents;
    end;
    if DoModal then
      Result := Frm.ShowModal <> mrCancel
    else
    begin
      Result := False;
      Frm.Show;
    end;
  finally
    if DoModal then
      FreeAndNil(Frm);
  end;
end;

function TfrmProgress.DoProgress: Boolean;
var
  AMin, AMax, APosition, AInterval: Integer;
  ACaption, ALabel: string;
begin
  Result := False;
  if FCancelled then
    Exit;
  Result := True;
  tmProgress.Enabled := False;
  if Assigned(FOnProgress) then
  begin
    AMin := pbProgress.Min;
    AMax := pbProgress.Max;
    APosition := pbProgress.Position;
    AInterval := tmProgress.Interval;
    ACaption := Caption;
    ALabel := Label1.Caption;
    FOnProgress(Self, AMin, AMax, APosition, AInterval, ACaption, ALabel, imProgress.Picture, Result);
    pbProgress.Min := AMin;
    pbProgress.Max := AMax;
    pbProgress.Position := APosition;
    tmProgress.Interval := AInterval;
    tmProgress.Enabled := AInterval > 0;
    Caption := ACaption;
    Label1.Caption := ALabel;
    AdjustComponents;
    Update;
  end;
  if not tmProgress.Enabled or not Result then
  begin
    ModalResult := mrCancel;
    Close;
  end;
end;

{$IFDEF VCL}

procedure TfrmProgress.AddCaption;
var
  WindowLong: Cardinal;
begin
  WindowLong := GetWindowLong(Handle, GWL_STYLE);
  if WindowLong and WS_CAPTION = 0 then
  begin
    SetWindowLong(Handle, GWL_STYLE, WindowLong or WS_CAPTION);
    BorderStyle := bsToolWindow;
    Height := Height + GetSystemMetrics(SM_CYCAPTION);
    Top := Top + GetSystemMetrics(SM_CYCAPTION);
    Update;
  end;
end;

procedure TfrmProgress.RemoveCaption;
var
  WindowLong: Cardinal;
begin
  WindowLong := GetWindowLong(Handle, GWL_STYLE);
  if WindowLong and WS_CAPTION = WS_CAPTION then
  begin
    BorderStyle := bsDialog;
    SetWindowLong(Handle, GWL_STYLE, WindowLong and not WS_CAPTION);
    Height := Height - GetSystemMetrics(SM_CYCAPTION);
    Top := Top - GetSystemMetrics(SM_CYCAPTION);
    Update;
  end;
end;

{$ENDIF VCL}

{$IFDEF VisualCLX}

procedure TfrmProgress.AddCaption;
var
  cw, ch: Integer;
begin
  if BorderStyle <> fbsToolWindow then
  begin
    cw := ClientWidth;
    ch := ClientHeight;
    BorderStyle := fbsToolWindow;
    Height := Height + ch - ClientHeight;
    Width := Width + cw - ClientWidth;
  end
end;

procedure TfrmProgress.RemoveCaption;
var
  cw, ch: Integer;
begin
  if BorderStyle <> fbsNone then
  begin
    cw := ClientWidth;
    ch := ClientHeight;
    BorderStyle := fbsNone;
    Height := Height + ch - ClientHeight;
    Width := Width + cw - ClientWidth;
  end
end;

{$ENDIF VisualCLX}

function TfrmProgress.ShowModal: Integer;
begin
  // (p3) put topmost but only if not debugging
  {$IFNDEF DEBUGINFO_ON}
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  {$ENDIF DEBUGINFO_ON}
  if not tmProgress.Enabled then
    DoProgress; // call at least once
  Result := inherited ShowModal;
end;

procedure TfrmProgress.tmProgressTimer(Sender: TObject);
begin
  if FCancelled then
    Exit;
  if not DoProgress or not tmProgress.Enabled then
  begin
    ModalResult := mrOk;
    Close;
  end;
end;

procedure TfrmProgress.btnCancelClick(Sender: TObject);
begin
  FCancelled := True;
  DoCancel;
end;

procedure TfrmProgress.AdjustComponents;
var
  Offset: Integer;
begin
  if Caption = '' then
    RemoveCaption
  else
    AddCaption;
  if (imProgress.Picture = nil) or (imProgress.Picture.Graphic = nil) or
    imProgress.Picture.Graphic.Empty then
    Offset := 12
  else
  begin
    Offset := imProgress.Top + imProgress.Height + 12;
    if ClientWidth - imProgress.Left * 2 < imProgress.Width then
      ClientWidth := imProgress.Width + imProgress.Left * 2;
  end;
  Label1.Top := Offset;
  Offset := Label1.Top + Label1.Height + 8;
  pbProgress.Top := Offset;
  Offset := pbProgress.Top + pbProgress.Height + 16;
  if btnCancel.Visible then
  begin
    btnCancel.Top := pbProgress.Top + pbProgress.Height + 16;
    Offset := btnCancel.Top + btnCancel.Height + 16;
  end;
  ClientHeight := Offset;
end;

procedure TfrmProgress.DoCancel;
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
  ModalResult := mrCancel;
  Close;
end;

procedure TfrmProgress.FormPaint(Sender: TObject);
begin
  if (imProgress.Picture.Graphic <> nil) and not imProgress.Picture.Graphic.Empty then
    Canvas.Draw(imProgress.Left, imProgress.Top, imProgress.Picture.Graphic);
end;

procedure TfrmProgress.Action1Execute(Sender: TObject);
begin
  if FCanClose then
    DoCancel;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

