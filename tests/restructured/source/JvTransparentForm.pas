{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTransparentForm.PAS, released on 2001-02-28.

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

unit JvTransparentForm;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, JvComponent;

type
  TJvTransparentForm = class(TJvComponent)
  private
    FMask: TBitmap;
    FComponentOwner: TCustomForm;
    FRegion: HRGN;
    FAutoSize: Boolean;
    FEnable: Boolean;
    procedure SetEnable(Value: Boolean);
    procedure SetMask(Value: TBitmap);
    procedure SetAutoSize(Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Mask: TBitmap read FMask write SetMask;
    property Enable: Boolean read FEnable write SetEnable;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
  end;

implementation
uses
  JvFunctions;
{***********************************************************}

constructor TJvTransparentForm.Create(AOwner: TComponent);
begin
  inherited;
  FComponentOwner := GetParentForm(TControl(AOwner));
  FMask := TBitmap.Create;
end;

{***********************************************************}

destructor TJvTransparentForm.Destroy;
begin
  if not (csDestroying in FComponentOwner.ComponentState) then
  begin
    SetWindowLong(FComponentOwner.Handle, GWL_STYLE,
      GetWindowLong(FComponentOwner.Handle, GWL_STYLE) or WS_CAPTION);
    SetWindowRgn(FComponentOwner.Handle, 0, True);
  end;
  FMask.Free;
  inherited;
end;

{***********************************************************}

procedure TJvTransparentForm.SetMask(Value: TBitmap);
begin
  FMask.Assign(Value);
  FRegion := RegionFromBitmap(Value);
  if FEnable then
    SetWindowRgn(FComponentOwner.Handle, FRegion, True)
  else
    SetWindowRgn(FComponentOwner.Handle, 0, True);
  if FEnable and FAutoSize then
  begin
    FComponentOwner.Width := FMask.Width;
    FComponentOwner.Height := FMask.Height;
  end;
end;

{***********************************************************}

procedure TJvTransparentForm.SetEnable(Value: Boolean);
begin
  FEnable := Value;
  if Value then
  begin
    SetWindowLong(FComponentOwner.Handle, GWL_style,
      GetWindowLong(FComponentOwner.Handle, GWL_style) and not WS_CAPTION);
    FRegion := RegionFromBitmap(FMask);
    SetWindowRgn(FComponentOwner.Handle, FRegion, True);
    if FAutoSize then
    begin
      FComponentOwner.Width := FMask.Width;
      FComponentOwner.Height := FMask.Height;
    end;
  end
  else
  begin
    SetWindowLong(FComponentOwner.Handle, GWL_STYLE,
      GetWindowLong(FComponentOwner.Handle, GWL_STYLE) or WS_CAPTION);
    SetWindowRgn(FComponentOwner.Handle, 0, True);
  end;
end;

{***********************************************************}

procedure TJvTransparentForm.SetAutoSize(Value: Boolean);
begin
  FAutoSize := Value;
  if Value and FEnable then
  begin
    FComponentOwner.Width := FMask.Width;
    FComponentOwner.Height := FMask.Height;
  end;
end;

end.
