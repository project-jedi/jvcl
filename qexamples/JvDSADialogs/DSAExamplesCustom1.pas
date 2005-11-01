{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit DSAExamplesCustom1;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, JvQComponent, JvQDSADialogs;

type
  TfrmDSAExamplesCustomDlg1 = class(TForm)
    JvDSADialog: TJvDSADialog;
    lblMessage: TLabel;
    cxSuppress: TCheckBox;
    btnOK: TButton;
    procedure FormResize(Sender: TObject);
  end;

procedure DoCustomDSA1;

implementation

{$R *.xfm}

procedure DoCustomDSA1;
begin
  with TfrmDSAExamplesCustomDlg1.Create(Screen.ActiveForm) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

{ TfrmDSAExamplesCustomDlg1 }

procedure TfrmDSAExamplesCustomDlg1.FormResize(Sender: TObject);
begin
  btnOK.Left := (ClientWidth - btnOK.Width) div 2;
end;

end.
