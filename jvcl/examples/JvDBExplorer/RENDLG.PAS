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

{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1996 AO ROSNO                       }
{                                                       }
{*******************************************************}

unit RenDlg;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Mask, JvToolEdit, JvExMask;

type
  TRenameDialog = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    TabnameEdit: TJvFilenameEdit ;
    Label1: TLabel;
    TabnameView: TEdit;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function RenameTableDialog(const CurrName, Directory: string;
  var NewName: string): Boolean;

implementation

{$R *.DFM}

function RenameTableDialog(const CurrName, Directory: string;
  var NewName: string): Boolean;
begin
  Result := False;
  NewName := CurrName;
  with TRenameDialog.Create(Application) do
  try
    TabnameView.Text := CurrName;
    TabnameEdit.Text := CurrName;
    TabnameEdit.InitialDir := Directory;
    if (ShowModal = mrOk) then begin
      NewName := TabnameEdit.Text;
      Result := (CurrName <> NewName);
    end;
  finally
    Free;
  end;
end;

end.
