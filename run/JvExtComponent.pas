{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExtComponent.pas, released on 2006-03-11.

The Initial Developer of the Original Code is Joe Doe .
Portions created by Joe Doe are Copyright (C) 1999 Joe Doe.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExtComponent;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Types,
  Classes, Graphics,
  JvExExtCtrls,
  JvExComCtrls;

type
  TJvPaintPanelContentEvent = procedure(Sender: TObject; Canvas: TCanvas; R: TRect) of object;

  TJvCustomPanel = class(TJvExCustomPanel)
  private
    FOnPaintContent: TJvPaintPanelContentEvent;
  protected
    function GetFlat: Boolean;
    procedure ReadCtl3D(Reader: TReader);
    procedure ReadParentCtl3D(Reader: TReader);
    procedure SetFlat(const Value: Boolean);
    function GetParentFlat: Boolean;
    procedure SetParentFlat(const Value: Boolean);

    procedure Paint; override;
    procedure PaintContent(const R: TRect); virtual;

    procedure DefineProperties(Filer: TFiler); override;

    property Flat: Boolean read GetFlat write SetFlat default False;
    property ParentFlat: Boolean read GetParentFlat write SetParentFlat default True;

    property OnPaintContent: TJvPaintPanelContentEvent read FOnPaintContent write FOnPaintContent;
  end;

  TJvPubCustomPanel = TJvExPubCustomPanel;
  TJvCustomTreeView = TJvExCustomTreeView;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

{ TJvCustomPanel }


function TJvCustomPanel.GetFlat: Boolean;
begin
  Result := not Ctl3D;
end;

function TJvCustomPanel.GetParentFlat: Boolean;
begin
  Result := ParentCtl3D;
end;

procedure TJvCustomPanel.SetFlat(const Value: Boolean);
begin
  Ctl3D := not Value;
end;

procedure TJvCustomPanel.SetParentFlat(const Value: Boolean);
begin
  ParentCtl3D := Value;
end;

procedure TJvCustomPanel.ReadCtl3D(Reader: TReader);
begin
  Flat := not Reader.ReadBoolean;
end;

procedure TJvCustomPanel.ReadParentCtl3D(Reader: TReader);
begin
  ParentFlat := Reader.ReadBoolean;
end;


procedure TJvCustomPanel.Paint;
begin
  inherited Paint;
  PaintContent(ClientRect);
end;

procedure TJvCustomPanel.PaintContent(const R: TRect);
begin
  if Assigned(FOnPaintContent) then
    FOnPaintContent(Self, Canvas, R);
end;

procedure TJvCustomPanel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('Ctl3D', ReadCtl3D, nil, False);
  Filer.DefineProperty('ParentCtl3D', ReadParentCtl3D, nil, False);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

{$IFDEF UNITVERSIONING}
finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.