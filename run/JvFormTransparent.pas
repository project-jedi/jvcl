{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTransparentForm.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFormTransparent;

{$I jvcl.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  JvComponent;

type
  TJvTransparentForm = class(TJvComponent)
  private
    FMask: TBitmap;
    FComponentOwner: TCustomForm;
    FAutoSize: Boolean;
    FActive: Boolean;
    procedure SetActive(Value: Boolean);
    procedure SetMask(Value: TBitmap);
  protected
    procedure SetAutoSize(Value: Boolean); virtual;
    procedure UpdateRegion;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // (rom) renamed to Active as (RB) asked for here
    property Active: Boolean read FActive write SetActive default False;
    property Mask: TBitmap read FMask write SetMask;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
  end;

implementation

uses
  JclGraphics;

// create a region from a bitmap
function RegionFromBitmap(const Image: TBitmap): HRGN;
begin
  Result := 0;
  if Assigned(Image) and not Image.Empty then
    Result := CreateRegionFromBitmap(Image, Image.Canvas.Pixels[0, 0], rmExclude);
end;

constructor TJvTransparentForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentOwner := GetParentForm(TControl(AOwner));
  FMask := TBitmap.Create;
end;

destructor TJvTransparentForm.Destroy;
begin
  if not (csDestroying in FComponentOwner.ComponentState) then
  begin
    { Enable caption }
    SetWindowLong(FComponentOwner.Handle, GWL_STYLE,
      GetWindowLong(FComponentOwner.Handle, GWL_STYLE) or WS_CAPTION);
    { Remove region }
    SetWindowRgn(FComponentOwner.Handle, 0, True);
  end;
  FMask.Free;
  inherited Destroy;
end;

procedure TJvTransparentForm.Loaded;
begin
  inherited Loaded;
  Mask := FMask;
  Active := FActive;
  AutoSize := FAutoSize;
end;

procedure TJvTransparentForm.SetMask(Value: TBitmap);
begin
  FMask.Assign(Value);
  if not (csLoading in ComponentState) then
    if Active then
      UpdateRegion
    else
      { Remove region }
      SetWindowRgn(FComponentOwner.Handle, 0, True);
end;

procedure TJvTransparentForm.SetActive(Value: Boolean);
begin
  FActive := Value;
  if not (csLoading in ComponentState) then
    if Value then
    begin
      { Remove caption }
      SetWindowLong(FComponentOwner.Handle, GWL_STYLE,
        GetWindowLong(FComponentOwner.Handle, GWL_STYLE) and not WS_CAPTION);
      { Set region }
      UpdateRegion;
    end
    else
    begin
     { Enable caption }
      SetWindowLong(FComponentOwner.Handle, GWL_STYLE,
        GetWindowLong(FComponentOwner.Handle, GWL_STYLE) or WS_CAPTION);
      { Remove region }
      SetWindowRgn(FComponentOwner.Handle, 0, True);
    end;
end;

procedure TJvTransparentForm.SetAutoSize(Value: Boolean);
begin
  FAutoSize := Value;
  if Value and Active and not (csLoading in ComponentState) then
  begin
    FComponentOwner.Width := FMask.Width;
    FComponentOwner.Height := FMask.Height;
  end;
end;

procedure TJvTransparentForm.UpdateRegion;
var
  Region: HRGN;
begin
  Region := RegionFromBitmap(FMask);
  if SetWindowRgn(FComponentOwner.Handle, Region, True) = 0 then
    DeleteObject(Region);
  { Region is now no longer valid }
  if FAutoSize then
  begin
    FComponentOwner.Width := FMask.Width;
    FComponentOwner.Height := FMask.Height;
  end;
end;

end.

