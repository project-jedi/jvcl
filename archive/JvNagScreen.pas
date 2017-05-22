{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvNagScreen.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvNagScreen;

interface

uses
  SysUtils, Classes, Graphics, Controls, ExtCtrls, Forms,
  JvBaseDlg, JvTypes;

type
  TJvNagScreen = class(TJvCommonDialogP)
  private
    FPict: TImage;
    FTimer: TTimer;
    FTimeToStay: Cardinal;
    FPicture: TBitmap;
    FForm: TForm;
    FDisappearOnClick: Boolean;
    FOnAfterNag: TNotifyEvent;
    FOnBeforeNag: TNotifyEvent;
    procedure NagCreate(Sender: TObject);
    procedure DisappearClick(Sender: TObject);
    procedure SetPicture(const Value: TBitmap);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Picture: TBitmap read FPicture write SetPicture;
    property TimeToStay: Cardinal read FTimeToStay write FTimeToStay default 5000;
    property DisappearOnClick: Boolean read FDisappearOnClick write FDisappearOnClick default True;
    procedure Execute; override;
    property OnBeforeNag: TNotifyEvent read FOnBeforeNag write FOnBeforeNag;
    property OnAfterNag: TNotifyEvent read FOnAfterNag write FOnAfterNag;
  end;

implementation

constructor TJvNagScreen.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TBitmap.Create;
  FTimeToStay := 5000;
  FDisappearOnClick := True;
end;

destructor TJvNagScreen.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TJvNagScreen.DisappearClick(Sender: TObject);
begin
  try
    FTimer.Enabled := False;
    FForm.Close;
    FTimer.Free;
    if Assigned(FOnAfterNag) then
      FOnAfterNag(Self);
  except
  end;
end;

procedure TJvNagScreen.NagCreate(Sender: TObject);
begin
  if (FPicture.Width <> 0) and (FPicture.Height <> 0) then
  begin
    FForm := TForm.Create(Application);
    FForm.Visible := False;
    FForm.BorderStyle := bsNone;
    FForm.Position := poScreenCenter;
    FForm.Caption := '';
    FForm.Width := FPicture.Width;
    FForm.Height := FPicture.Height;
    FPict := TImage.Create(FForm);
    FPict.Align := alClient;
    FPict.AutoSize := True;
    FPict.Picture.Bitmap.Assign(FPicture);
    FPict.Parent := FForm;
    FPict.Width := FPicture.Width;
    FPict.Height := FPicture.Height;
    FPict.Left := 0;
    FPict.Top := 0;
    FPict.Visible := True;

    if FDisappearOnClick then
      FPict.OnClick := DisappearClick;

    FTimer := TTimer.Create(Self);
    FTimer.Interval := FTimeToStay;
    FTimer.OnTimer := DisappearClick;
    FTimer.Enabled := True;

    if Assigned(FOnBeforeNag) then
      FOnBeforeNag(Self);

    FForm.ShowModal;
    FForm.Free;
  end;
end;

procedure TJvNagScreen.Execute;
begin
  NagCreate(Self);
end;

procedure TJvNagScreen.Loaded;
begin
  if not (csDesigning in ComponentState) then
    Execute;
end;

procedure TJvNagScreen.SetPicture(const Value: TBitmap);
begin
  FPicture.Assign(Value);
end;

end.

