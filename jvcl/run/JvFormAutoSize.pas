{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAutoSizeCompo.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFormAutoSize;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, StdCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QStdCtrls,
  {$ENDIF VisualCLX}
  JvComponent;

type
  TJvFormAutoSize = class(TJvComponent)
  private
    FForm: TForm;
    FActive: Boolean;
    FResize: TNotifyEvent;
    FOldWidth: Integer;
    FOldHeight: Integer;
    procedure Resize(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // (p3) default here should be false!!!
    property Active: Boolean read FActive write FActive default False;
  end;

implementation

constructor TJvFormAutoSize.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // (p3) dangerous: True can create problems without user being aware
  FActive := False;
  FForm := GetParentForm(TControl(AOwner)) as TForm;
  if FForm <> nil then
  begin
    FOldWidth := FForm.Width;
    FOldHeight := FForm.Height;
    FResize := FForm.OnResize;
    FForm.OnResize := Resize;
  end;
end;

destructor TJvFormAutoSize.Destroy;
begin
  if FForm <> nil then
    FForm.OnResize := nil;
  FForm := nil;
  inherited Destroy;
end;

procedure TJvFormAutoSize.Resize(Sender: TObject);
var
  WidthRatio, HeightRatio: Double;
  CompIndex: Integer;
begin
  if FForm = nil then
    FForm := GetParentForm(Owner as TControl) as TForm;
  if FActive and (FForm <> nil) then
  begin
    // (p3) this code is slightly dangerous: no sanity checks -
    // values can become really large or really small
    if (FOldWidth <> 0) and (FOldHeight <> 0) then
    begin
      WidthRatio := FForm.Width / FOldWidth;
      HeightRatio := FForm.Height / FOldHeight;
      for CompIndex := 0 to FForm.ComponentCount - 1 do
      begin
        if FForm.Components[CompIndex] is TControl then
        begin
          with FForm.Components[CompIndex] as TControl do
          begin
            if not (FForm.Components[CompIndex] is TButton) then
            begin
              Width := Round(Width * WidthRatio);
              Height := Round(Height * HeightRatio);
            end;
            Left := Round(Left * WidthRatio);
            Top := Round(Top * HeightRatio);
          end;
        end;
      end;
    end;
    FOldWidth := FForm.Width;
    FOldHeight := FForm.Height;
  end;
  if Assigned(FResize) then
    FResize(Sender);
end;

end.
