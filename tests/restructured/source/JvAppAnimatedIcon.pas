{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppAnimatedIcon.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvAppAnimatedIcon;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, JvComponent;

type
  TJvAppAnimatedIcon = class(TJvComponent)
  private
    FActive: Boolean;
    FDelay: Cardinal;
    FImgList: TImageList;
    FTimer: TTimer;
    FNumber: Integer;
    procedure SetActive(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure SetImgList(const Value: TImageList);
    procedure Animate(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Icons: TImageList read FImgList write SetImgList;
    property Active: Boolean read FActive write SetActive;
    property Delay: Cardinal read FDelay write SetDelay default 100;
  end;

implementation

{**************************************************}

constructor TJvAppAnimatedIcon.Create(AOwner: TComponent);
begin
  inherited;
  FNumber := 0;
  FDelay := 100;
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := Animate;
  FTimer.Interval := FDelay;
  FTimer.Enabled := FActive;
end;

{**************************************************}

destructor TJvAppAnimatedIcon.Destroy;
begin
  FTimer.Free;
  inherited;
end;

{**************************************************}

procedure TJvAppAnimatedIcon.Animate(Sender: TObject);
begin
  if (FImgList <> nil) and (FImgList.Count <> 0) then
  begin
    FNumber := (FNumber + 1) mod FimgList.Count;
    FImgList.GetIcon(FNumber, Application.Icon);
  end;
end;

{**************************************************}

procedure TJvAppAnimatedIcon.SetActive(const Value: Boolean);
begin
  FActive := Value;
  FTimer.Enabled := FActive;
end;

{**************************************************}

procedure TJvAppAnimatedIcon.SetDelay(const Value: Cardinal);
begin
  FDelay := Value;
  FTimer.Interval := FDelay;
end;

{**************************************************}

procedure TJvAppAnimatedIcon.SetImgList(const Value: TImageList);
begin
  FImgList := Value;
end;

end.
