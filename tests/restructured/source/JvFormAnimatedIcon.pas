{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormAnimatedIcon.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFormAnimatedIcon;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, JvComponent;

type
  TJvFormAnimatedIcon = class(TJvComponent)
  private
    FForm: TCustomForm;
    FActive: Boolean;
    FDelay: Cardinal;
    FImgList: TImageList;
    FTimer: TTimer;
    FNumber: Integer;
    procedure SetActive(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure Animate(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Icons: TImageList read FImgList write FImgList;
    property Active: Boolean read FActive write SetActive;
    property Delay: Cardinal read FDelay write SetDelay default 100;
  end;

implementation

{**************************************************}

constructor TJvFormAnimatedIcon.Create(AOwner: TComponent);
begin
  inherited;

  FForm := GetParentForm(TControl(AOwner));
  FNumber := 0;
  FDelay := 100;

  FTimer := nil;
  if not (csDesigning in ComponentState) then
  begin
    FTimer := TTimer.Create(Self);
    FTimer.OnTimer := Animate;
    FTimer.Interval := FDelay;
    FTimer.Enabled := Factive;
  end;
end;

{**************************************************}

destructor TJvFormAnimatedIcon.Destroy;
begin
  FTimer.Free;
  inherited;
end;

{**************************************************}

procedure TJvFormAnimatedIcon.Animate(Sender: TObject);
begin
  if (FImglist <> nil) and (FImgList.Count <> 0) then
  begin
    FNumber := (FNumber + 1) mod FimgList.Count;
    FimgList.GetIcon(FNumber, TForm(FForm).Icon);
  end;
end;

{**************************************************}

procedure TJvFormAnimatedIcon.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if not (csDesigning in ComponentState) then
    FTimer.Enabled := FActive;
end;

{**************************************************}

procedure TJvFormAnimatedIcon.SetDelay(const Value: Cardinal);
begin
  FDelay := Value;
  if not (csDesigning in ComponentState) then
    FTimer.Interval := FDelay;
end;

end.
