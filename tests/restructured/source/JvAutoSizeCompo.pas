{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAutoSizeCompo.PAS, released on 2001-02-28.

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

unit JvAutoSizeCompo;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, JvComponent;

type
  TJvAutoSizeCompo = class(TJvComponent)
  private
    FForm: TForm;
    FActive: Boolean;
    FResize: TNotifyEvent;
    FOldWidth: Integer;
    FOldHeight: Integer;
    procedure Resize(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write FActive default True;
  end;

implementation

{**************************************************}

constructor TJvAutoSizeCompo.Create(AOwner: TComponent);
begin
  inherited;
  FActive := True;
  FForm := TForm(GetParentForm(TControl(AOwner)));
  FOldWidth := FForm.Width;
  FOldHeight := FForm.Height;
  FResize := FForm.OnResize;
  FForm.OnResize := Resize;
end;
{**************************************************}

destructor TJvAutoSizeCompo.Destroy;
begin
  if FForm <> nil then
    FForm.OnResize := nil;
  inherited;
end;
{**************************************************}

procedure TJvAutoSizeCompo.Resize(Sender: TObject);
var
  WidthRatio, HeightRatio: Double;
  CompIndex: Integer;
begin
  if FActive then
  begin
    if (FOldWidth <> 0) and (FOldHeight <> 0) then
    begin
      WidthRatio := FForm.Width / FOldWidth;
      HeightRatio := FForm.Height / FOldHeight;
      for CompIndex := 0 to (FForm.ComponentCount - 1) do
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
