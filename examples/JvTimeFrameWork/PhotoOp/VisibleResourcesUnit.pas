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

unit VisibleResourcesUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, checklst;

type
  TVisibleResources = class(TForm)
    ResourcesCheckList: TCheckListBox;
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VisibleResources: TVisibleResources;

implementation

Uses PhotoOpUnit;

{$R *.DFM}

procedure TVisibleResources.FormShow(Sender: TObject);
var
  I : Integer;
begin
  // Check the template and check any resources currently
  // visible in the grid
  With PhotoOpMain.JvTFDays1.Template do
    For I := 0 to ResourcesCheckList.Items.Count - 1 do
      ResourcesCheckList.Checked[I] :=
        CompNames.IndexOf(ResourcesCheckList.Items[I]) > -1;
end;

procedure TVisibleResources.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  I : Integer;
begin
  If ModalResult = mrOK Then
  begin
      // First, clear the old resources from the template
      PhotoOpMain.JvTFDays1.Template.CompNames.Clear;
      PhotoOpMain.JvTFWeeks1.SchedNames.Clear;
      PhotoOpMain.JvTFMonths1.SchedNames.Clear;

      // Next, add the new resources to the template
      For I := 0 to ResourcesCheckList.Items.Count - 1 do
         If ResourcesCheckList.Checked[I] Then
         begin
             PhotoOpMain.JvTFDays1.Template.CompNames.Add(ResourcesCheckList.Items[I]);
             PhotoOpMain.JvTFWeeks1.SchedNames.Add(ResourcesCheckList.Items[I]);
             PhotoOpMain.JvTFMonths1.SchedNames.Add(ResourcesCheckList.Items[I]);
         end;
  end;
end;

end.
