{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCompEditorTemplate.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Michael Beck [mbeck@bigfoot.com]
Portions created by Michael Beck are Copyright (C) 2003 Michael Beck
All Rights Reserved.

Contributor(s):

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}

unit JvCompEditorTemplateForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, ExtCtrls,
  {$IFDEF COMPILER6_UP}
  DesignIntf,
  DesignEditors,
  PropertyCategories,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvBaseDlg, JvJVCLAbout, JvRadioControl, JvComponent,
  JvgCheckBox, JvgLanguageLoader, JvgLabel, JvgGroupBox, JvgTab, JvgPage,
  JvgShadow, JvgWizardHeader, JvgBitBtn, JvgSpeedButton, JvgSplit;

type
  TJvgCompEditorTemplate = class(TForm)
    pnMain: TPanel;
    pnBottom: TPanel;
    glShadowOK: TJvgShadow;
    glShadowCancel: TJvgShadow;
    btnCancel1: TSpeedButton;
    btnOK1: TSpeedButton;
    pgMain: TJvgPageControl;
    tabMain: TTabSheet;
    hwJVCLCompEditor: TJvgWizardHeader;
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

{$R *.DFM}

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
        Form.ModalResult := mrOK;
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

