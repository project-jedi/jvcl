{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormAnimatedIcon.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFormAnimatedIcon;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Controls, Forms, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QForms, QExtCtrls, QImgList, QGraphics,
  {$ENDIF VisualCLX}
  JvComponent;

type
  TJvFormAnimatedIcon = class(TJvComponent)
  private
    FForm: TCustomForm;
    FActive: Boolean;
    FDelay: Cardinal;
    FIcons: TImageList;
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
    property Icons: TImageList read FIcons write FIcons;
    property Active: Boolean read FActive write SetActive default True;
    property Delay: Cardinal read FDelay write SetDelay default 100;
  end;

implementation

constructor TJvFormAnimatedIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FForm := GetParentForm(TControl(AOwner));
  FActive := True;
  FNumber := 0;
  FDelay := 100;

  FTimer := nil;
  if not (csDesigning in ComponentState) then
  begin
    FTimer := TTimer.Create(Self);
    FTimer.OnTimer := Animate;
    FTimer.Interval := FDelay;
    FTimer.Enabled := FActive;
  end;
end;

destructor TJvFormAnimatedIcon.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TJvFormAnimatedIcon.Animate(Sender: TObject);
{$IFDEF VisualCLX}
var
  Bmp: TBitmap;
{$ENDIF VisualCLX}
begin
  if (FIcons <> nil) and (FIcons.Count <> 0) then
  begin
    FNumber := (FNumber + 1) mod FIcons.Count;
    {$IFDEF VCL}
    FIcons.GetIcon(FNumber, TForm(FForm).Icon);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    Bmp := TBitmap.Create;
    FIcons.GetBitmap(FNumber, Bmp);
    TForm(FForm).Icon.Assign(Bmp);
    Bmp.Free;
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvFormAnimatedIcon.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if not (csDesigning in ComponentState) then
    FTimer.Enabled := FActive;
end;

procedure TJvFormAnimatedIcon.SetDelay(const Value: Cardinal);
begin
  FDelay := Value;
  if not (csDesigning in ComponentState) then
    FTimer.Interval := FDelay;
end;

end.

