{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMaxPixel.PAS, released on 2001-02-28.

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

unit JvMaxPixel;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Forms;

type
  TJvMaxPixel = class(TPersistent)
  private
    FUseControl: Boolean;
    FLength: Integer;
    FFont: TFont;
    FOnChanged: TNotifyEvent;
    FParent: TControl;
    procedure SetFont(const Value: TFont);
    procedure SetLength(const Value: Integer);
    procedure SetUseControl(const Value: Boolean);
  protected
    procedure FontChanged(Sender: TObject);
    procedure Changed;
  public
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure Test(var Value: string; ParentFont: TFont);
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
  published
    property Length: Integer read FLength write SetLength default 0;
    property UseControlFont: Boolean read FUseControl write SetUseControl default True;
    property Font: TFont read FFont write SetFont;
  end;

implementation

{***********************************************}

procedure TJvMaxPixel.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{***********************************************}

constructor TJvMaxPixel.Create(AOwner: TControl);
begin
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FUseControl := True;
  FLength := 0;
  FParent := AOwner;
end;

{***********************************************}

destructor TJvMaxPixel.Destroy;
begin
  FFont.Free;
  inherited;
end;

{***********************************************}

procedure TJvMaxPixel.FontChanged(Sender: TObject);
begin
  Changed;
end;

{***********************************************}

procedure TJvMaxPixel.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

{***********************************************}

procedure TJvMaxPixel.SetLength(const Value: Integer);
begin
  FLength := Value;
  Changed;
end;

{***********************************************}

procedure TJvMaxPixel.SetUseControl(const Value: Boolean);
begin
  FUseControl := Value;
  Changed;
end;

{***********************************************}

procedure TJvMaxPixel.Test(var Value: string; ParentFont: TFont);
begin
  if FLength = 0 then
    Exit;

  with TControlCanvas.Create do
  begin
    Control := FParent;
    if FUseControl then
      Font.Assign(ParentFont)
    else
      Font.Assign(FFont);

    if TextWidth(Value) > FLength then
      Beep;
    while (TextWidth(Value) > FLength) and (Value <> '') do
      Value := Copy(Value, 1, System.Length(Value) - 1);
    Free;
  end;
end;

end.
