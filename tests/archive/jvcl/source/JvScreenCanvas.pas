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

{$I JVCL.INC}

unit JvScreenCanvas;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms;

type
  TJvScreenCanvas = class(TObject)
  private
    FCanvas: TCanvas;
    FPainting: Boolean;
    FBidon: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetCanvas: TCanvas;
  public
    constructor Create;
    destructor Destroy;override;
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

constructor TJvScreenCanvas.Create;
begin
  inherited;
  FPainting := False;
end;

{************************************************************}

destructor TJvScreenCanvas.Destroy;
begin
  EndPaint;
  inherited;
end;

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
