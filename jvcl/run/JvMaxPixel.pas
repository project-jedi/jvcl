{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMaxPixel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvMaxPixel;

{$I jvcl.inc}

interface

uses
  SysUtils, Graphics, Controls, Classes;

type
  TJvMaxPixel = class(TPersistent)
  private
    FUseControlFont: Boolean;
    FLength: Integer;
    FFont: TFont;
    FOnChanged: TNotifyEvent;
    FParent: TControl;
    function IsFontStored: Boolean;
    procedure SetFont(const Value: TFont);
    procedure SetLength(const Value: Integer);
    procedure SetUseControlFont(const Value: Boolean);
  protected
    procedure FontChanged(Sender: TObject);
    procedure Changed;
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
    function Test(var Value: string; ParentFont: TFont): Boolean;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Length: Integer read FLength write SetLength default 0;
    property UseControlFont: Boolean read FUseControlFont write SetUseControlFont default True;
    property Font: TFont read FFont write SetFont stored IsFontStored;
  end;

implementation

constructor TJvMaxPixel.Create(AOwner: TControl);
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FUseControlFont := True;
  FLength := 0;
  FParent := AOwner;
end;

destructor TJvMaxPixel.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TJvMaxPixel.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TJvMaxPixel.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TJvMaxPixel.SetFont(const Value: TFont);
begin
  if Value <> FFont then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TJvMaxPixel.SetLength(const Value: Integer);
begin
  if Value <> FLength then
  begin
    FLength := Value;
    Changed;
  end;
end;

procedure TJvMaxPixel.SetUseControlFont(const Value: Boolean);
begin
  if Value <> FUseControlFont then
  begin
    FUseControlFont := Value;
    Changed;
  end;
end;

function TJvMaxPixel.Test(var Value: string; ParentFont: TFont): Boolean;
begin
  Result := True;
  if Length = 0 then
    Exit;

  with TControlCanvas.Create do
    try
      Result := False;
      Control := FParent;
      if FUseControlFont then
        Font.Assign(ParentFont)
      else
        Font.Assign(FFont);

      Result := TextWidth(Value) > Length;
      while (TextWidth(Value) > Length) and (Value <> '') do
        Value := Copy(Value, 1, System.Length(Value) - 1);
    finally
      Free;
    end;
end;

function TJvMaxPixel.IsFontStored: Boolean;
begin
  Result := not UseControlFont;
end;

end.

