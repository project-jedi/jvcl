{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProgressFrm.PAS, released on 2003-03-31.

The Initial Developer of the Original Code is Peter Thörnqvist.
Portions created by Peter Thörnqvist are Copyright (c) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-03-31

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

Description:
- This form is used by JvProgressDialog.pas
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvProgressFrm;

interface

uses
  Windows, SysUtils, Forms, Graphics, ExtCtrls, StdCtrls, Controls, ComCtrls,
  Classes;

type
  TJvPrivateProgressUpdate = procedure (Sender:TObject; var AMin, AMax, APosition, AInterval:integer;
    var ACaption,ALabel:string; AnImage:TPicture; var AContinue:boolean) of object;
  TfrmProgress = class(TForm)
    pbProgress: TProgressBar;
    imProgress: TImage;
    Label1: TLabel;
    btnCancel: TButton;
    tmProgress: TTimer;
    procedure tmProgressTimer(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
    FOnProgress:TJvPrivateProgressUpdate;
    FOnCancel:TNotifyEvent;
    FCancelled:boolean;
    function DoProgress:boolean;
    procedure DoCancel;

    procedure AdjustComponents;
    procedure RemoveCaption;
    procedure AddCaption;
  public
    { Public declarations }
    class function Execute(frm:TfrmProgress;const ACaption,ALabel:string;
      AImage:TPicture=nil;ATransparent:boolean=false;AMin:integer=0;AMax:integer=100;APosition:integer=0;AInterval:integer=200;ShowCancel:boolean=false;AOnProgress:TJvPrivateProgressUpdate=nil;AOnCancel:TNotifyEvent=nil):boolean;
    function ShowModal: Integer; override;
  end;

implementation
uses
  Consts;

{$R *.dfm}

{ TfrmProgress }

class function TfrmProgress.Execute(frm:TfrmProgress;const ACaption, ALabel: string;
  AImage: TPicture; ATransparent:boolean;AMin, AMax, APosition, AInterval: integer;ShowCancel:boolean;AOnProgress:TJvPrivateProgressUpdate;AOnCancel:TNotifyEvent): boolean;
var DoModal:boolean;
begin
  if frm = nil then
  begin
    frm := self.Create(Application);
    DoModal := true;
  end
  else
    DoModal := false;
  try
    frm.Caption := ACaption;
    frm.Label1.Caption := ALabel;
    frm.pbProgress.Min := AMin;
    frm.pbProgress.Max := AMax;
    frm.pbProgress.Position := APosition;
    frm.FOnProgress := AOnProgress;
    frm.imProgress.Picture := AImage;
    frm.imProgress.Transparent := ATransparent;
    frm.tmProgress.Interval := AInterval;
    frm.tmProgress.Enabled := AInterval > 0;
    frm.btnCancel.Visible := ShowCancel;
    frm.btnCancel.Caption := SCancelButton;
    frm.FOnCancel := AOnCancel;
    frm.AdjustComponents;
    if DoModal then
      Result := frm.ShowModal <> mrCancel
    else
    begin
      Result := false;
      frm.Show;
    end;
  finally
    if DoModal then
      FreeAndNil(frm);
  end;
end;

function TfrmProgress.DoProgress: boolean;
var AMin,AMax, APosition,AInterval:integer;ACaption,ALabel:string;
begin
  Result := false;
  if FCancelled then Exit;
  Result := true;
  tmProgress.Enabled := false;
  if Assigned(FOnProgress) then
  begin
    AMin := pbProgress.Min;
    AMax := pbProgress.Max;
    APosition := pbProgress.Position;
    AInterval := tmProgress.Interval;
    ACaption := Caption;
    ALabel    := Label1.Caption;
    FOnProgress(self,AMin,AMax,APosition,AInterval,ACaption,ALabel,imProgress.Picture,Result);
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
procedure TfrmProgress.AddCaption;
var WindowLong:Cardinal;
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
var WindowLong:Cardinal;
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

function TfrmProgress.ShowModal: Integer;
begin
  // (p3) put topmost but only if not debugging
  {$IFOPT D-}
    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  {$ENDIF}
  if not tmProgress.Enabled then
    DoProgress; // call at least once
  Result := inherited ShowModal;
end;

procedure TfrmProgress.tmProgressTimer(Sender: TObject);
begin
  if FCancelled then Exit;
  if not DoProgress or not tmProgress.Enabled then
  begin
    ModalResult := mrOk;
    Close;
  end;
end;

procedure TfrmProgress.btnCancelClick(Sender: TObject);
begin
  FCancelled := true;
  DoCancel;
end;

procedure TfrmProgress.AdjustComponents;
var Offset:integer;
begin
  if Caption = '' then RemoveCaption else AddCaption; 
  if (imProgress.Picture = nil) or (imProgress.Picture.Graphic = nil)
    or imProgress.Picture.Graphic.Empty then
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
  if Assigned(FOnCancel) then FOnCancel(self);
  ModalResult := mrCancel;
  Close;
end;

procedure TfrmProgress.FormPaint(Sender: TObject);
begin
  if (imProgress.Picture.Graphic <> nil) and not imProgress.Picture.Graphic.Empty then
    Canvas.Draw(imProgress.Left,imProgress.Top,imProgress.Picture.Graphic);
end;


end.
