{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
unit ContentsFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls, ImgList,
  JvComponent;

type
  TContentsForm = class(TForm)
    lbDockClient1: TJvDockClient;
    TreeView1: TTreeView;
    Panel1: TPanel;
    Label2: TLabel;
    FilteredbyComboBox: TComboBox;
    ImageList1: TImageList;
    procedure Panel1Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ContentsForm: TContentsForm;

implementation

uses MSDN2002MainUnit;

{$R *.DFM}

procedure TContentsForm.Panel1Resize(Sender: TObject);
begin
  FilteredbyComboBox.Width := Panel1.ClientWidth - 0;
end;

procedure TContentsForm.FormCreate(Sender: TObject);
begin
  FilteredbyComboBox.ItemIndex := 0;
end;

end.
