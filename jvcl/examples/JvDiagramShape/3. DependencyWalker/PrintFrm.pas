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

unit PrintFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DepWalkConsts, PersistForm, PersistSettings;

type
  TfrmPrint = class(TfrmPersistable)
    Label1: TLabel;
    cbFormat: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
  private
    { Private declarations }
  protected
    { IPersistSettings}
    procedure Load(Storage:TPersistStorage);override;
    procedure Save(Storage:TPersistStorage);override;
  public
    { Public declarations }
    class function Execute:boolean;
  end;


implementation

{$R *.dfm}

{ TfrmPrint }

class function TfrmPrint.Execute: boolean;
var Storage:TPersistStorage;
begin
  with self.Create(Application) do
  try
    Storage := GetStorage;
    try
      Load(Storage);
      Result := ShowModal = mrOK;
      if Result then
      begin
        Save(Storage);
        Storage.UpdateFile;
      end;
    finally
      Storage.Free;
    end;
  finally
    Free;
  end;
end;

procedure TfrmPrint.Load(Storage: TPersistStorage);
begin
  inherited;
  cbFormat.ItemIndex := Storage.ReadInteger('Printing','Print Format',0);
end;

procedure TfrmPrint.Save(Storage: TPersistStorage);
begin
  inherited;
  Storage.WriteInteger('Printing','Print Format',cbFormat.ItemIndex);
end;

end.
