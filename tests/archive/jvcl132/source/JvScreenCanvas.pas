{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScreenCanvas.PAS, released on 2001-02-28.

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

unit JvScreenCanvas;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, JvComponent;

type
  TJvScreenCanvas = class(TJvComponent)
  private
    FCanvas: TCanvas;
    FPainting: Boolean;
    FBidon: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetCanvas: TCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas: TCanvas read GetCanvas;
  published
    property Height: Integer read GetHeight write FBidon;
    property Width: Integer read GetWidth write FBidon;
    procedure BeginPaint;
    procedure EndPaint;
  end;

implementation

{************************************************************}

procedure TJvScreenCanvas.BeginPaint;
var
  DC: HDC;
begin
  if FPainting then
    Exit;
  try
    FCanvas := TCanvas.Create;
    DC := GetDC(0);
    FCanvas.Handle := DC;
    FPainting := True;
  except
    FPainting := False;
  end;
end;

{************************************************************}

constructor TJvScreenCanvas.Create(AOwner: TComponent);
begin
  inherited;
  FPainting := False;
end;

{************************************************************}

procedure TJvScreenCanvas.EndPaint;
begin
  if not FPainting then
    Exit;
  ReleaseDc(0, FCanvas.Handle);
  FCanvas.Free;
  FPainting := False;
end;

{************************************************************}

function TJvScreenCanvas.GetCanvas: TCanvas;
begin
  if FPainting then
    Result := FCanvas
  else
    Result := nil;
end;

{************************************************************}

function TJvScreenCanvas.GetHeight: Integer;
begin
  Result := Screen.Height;
end;

{************************************************************}

function TJvScreenCanvas.GetWidth: Integer;
begin
  Result := Screen.Width;
end;

end.
