{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImageDlg.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQImageDlg;

interface

uses
  SysUtils, Classes,
  
  
  QGraphics, QControls, QForms, QExtCtrls,
  
  JvQBaseDlg, JvQComponent, JvQTypes;

type
  TJvImageDialog = class(TJvCommonDialogP)
  private
    FPicture: TPicture;
    FTitle: string;
    function GetPicture: TPicture;
    procedure SetPicture(const Value: TPicture);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Picture: TPicture read GetPicture write SetPicture;
    property Title: string read FTitle write FTitle;
    procedure Execute; override;
  end;

implementation

uses
  JvQResources, JvQExForms;

constructor TJvImageDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := nil;
  FTitle := RsImageTitle;
end;

destructor TJvImageDialog.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TJvImageDialog.Execute;
var
  Form: TJvForm;
  Image1: TImage;
begin
  if (Picture.Height <> 0) and (Picture.Width <> 0) then
  begin
    Form := TJvForm.CreateNew(Self);
    try
      
      
      Form.BorderStyle := fbsDialog;
      
      Form.BorderIcons := [biSystemMenu];
      Form.Position := poScreenCenter;
      Image1 := TImage.Create(Form);
      Image1.Picture.Assign(Picture);
      Image1.Parent := Form;
      Form.ClientHeight := Image1.Height;
      Form.ClientWidth := Image1.Width;
      Form.Caption := FTitle;
      Form.ShowModal;
    finally
      Form.Free;
    end;
  end;
end;

function TJvImageDialog.GetPicture: TPicture;
begin
  if FPicture = nil then
    FPicture := TPicture.Create;
  Result := FPicture;
end;

procedure TJvImageDialog.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

end.

