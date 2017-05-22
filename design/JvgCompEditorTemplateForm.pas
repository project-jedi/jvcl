{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCompEditorTemplate.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Michael Beck [mbeck att bigfoot dott com]
Portions created by Michael Beck are Copyright (C) 2003 Michael Beck
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgCompEditorTemplateForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, ExtCtrls,
  DesignIntf, DesignEditors, PropertyCategories;

type
  TJvgCompEditorTemplate = class(TForm)
    pnMain: TPanel;
    pnBottom: TPanel;
    btnCancel1: TButton;
    btnOK1: TButton;
    pgMain: TPageControl;
    tabMain: TTabSheet;
    procedure FormShow(Sender: TObject);
    procedure btnCancel1Click(Sender: TObject); virtual;
    procedure btnOK1Click(Sender: TObject); virtual;
  protected
    procedure InitializeEditor; virtual;
    function UpdateComponent: Boolean; virtual;
  end;

var
  JvgCompEditorTemplate: TJvgCompEditorTemplate;

implementation

{$R *.dfm}

procedure TJvgCompEditorTemplate.FormShow(Sender: TObject);
begin
  InitializeEditor;
end;

procedure TJvgCompEditorTemplate.btnCancel1Click(Sender: TObject);
begin
  Close;
end;

procedure TJvgCompEditorTemplate.btnOK1Click(Sender: TObject);
var
  Form: TCustomForm;
begin
  if UpdateComponent then
    begin
      Form := GetParentForm(Self);
      if Form <> nil then
        Form.ModalResult := mrOk;
    end;
end;

function TJvgCompEditorTemplate.UpdateComponent: Boolean;
begin
  Result := False;
end;

procedure TJvgCompEditorTemplate.InitializeEditor;
begin
end;

end.
