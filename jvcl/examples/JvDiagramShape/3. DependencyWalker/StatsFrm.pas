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

unit StatsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, PersistForm, PersistSettings;

type
  TfrmUnitStats = class(TfrmPersistable)
    Label1: TLabel;
    edName: TEdit;
    Label2: TLabel;
    reUsed: TRichEdit;
    Label3: TLabel;
    reUses: TRichEdit;
    btnOK: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure Execute(const UnitName: string; UsedByStrings, UsesStrings: TStrings);
  end;


implementation

{$R *.dfm}

{ TfrmUnitStats }

class procedure TfrmUnitStats.Execute(const UnitName: string; UsedByStrings,
  UsesStrings: TStrings);
var Storage: TPersistStorage;
begin
  with self.Create(Application) do
  try
    Storage := GetStorage;
    try
      Load(Storage);
      Caption := Format(Caption, [ExtractFilename(UnitName)]);
      edName.Text := UnitName;
      reUsed.Lines := UsedByStrings;
      reUses.Lines := UsesStrings;
      ShowModal;
      Save(Storage);
      Storage.UpdateFile;
    finally
      Storage.Free;
    end;
  finally
    Free;
  end;
end;


end.

