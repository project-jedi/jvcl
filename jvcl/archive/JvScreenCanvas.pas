{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScreenCanvas.PAS, released on 2001-02-28.

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

unit JvScreenCanvas;

interface

uses
  Windows, SysUtils, Graphics, Forms;

type
  TJvScreenCanvas = class(TObject)
  private
    FCanvas: TCanvas;
    FPainting: Boolean;
    FDummy: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
  published
    property Height: Integer read GetHeight write FDummy;
    property Width: Integer read GetWidth write FDummy;
    procedure BeginPaint;
    procedure EndPaint;
  end;

implementation

constructor TJvScreenCanvas.Create;
begin
  inherited Create;
  FPainting := False;
  FCanvas := nil;
end;

destructor TJvScreenCanvas.Destroy;
begin
  EndPaint;
  inherited Destroy;
end;

procedure TJvScreenCanvas.BeginPaint;
begin
  if FPainting then
    Exit;
  try
    FCanvas := TCanvas.Create;
    FCanvas.Handle := GetDC(HWND_DESKTOP);
    FPainting := True;
  except
    FPainting := False;
  end;
end;

procedure TJvScreenCanvas.EndPaint;
begin
  if not FPainting then
    Exit;
  ReleaseDC(HWND_DESKTOP, FCanvas.Handle);
  FCanvas.Free;
  FCanvas := nil;
  FPainting := False;
end;

function TJvScreenCanvas.GetHeight: Integer;
begin
  Result := Screen.Height;
end;

function TJvScreenCanvas.GetWidth: Integer;
begin
  Result := Screen.Width;
end;

end.

