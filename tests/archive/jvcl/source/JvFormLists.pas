{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormLists.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFormLists;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, JvBitBtn, JvLabel;

type
  TFormListb = class(TForm)
    ListBox1: TListBox;
    ListBox2: TListBox;
    Bevel1: TBevel;
    lblColumn1: TJvLabel;
    lblColumn2: TJvLabel;
    btnLeft: TButton;
    btnRight: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnLeftClick(Sender: TObject);
    procedure btnRightClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class function Execute(const ACaption,AColCaption1,AColCaption2:string;List1,List2:TStrings):boolean;
  end;

implementation

{$R *.DFM}

{$IFNDEF COMPILER6_UP}

procedure CopySelected(De,Vers:TListbox);
var
   i:Integer;
begin
   i:=0;
   while i<de.Items.count do
   begin
      if de.Selected[i] then
      begin
         vers.Items.AddObject(de.items[i],de.items.objects[i]);
         de.Items.delete(i);
      end
      else inc(i);
   end;
end;



{$ENDIF}


procedure TFormListb.btnLeftClick(Sender: TObject);
begin
{$IFDEF COMPILER6_UP}
listbox2.CopySelection(ListBox1);
  while ListBox2.SelCount > 0 do
    ListBox2.DeleteSelected;

{$ELSE}
   CopySelected(self.listbox2,self.listbox1);
 {$ENDIF}


end;

procedure TFormListb.btnRightClick(Sender: TObject);
begin
{$IFDEF COMPILER6_UP}
  ListBox1.CopySelection(ListBox2);
  while ListBox1.SelCount > 0 do
    ListBox1.DeleteSelected;
{$ELSE}
   CopySelected(self.listbox1,self.listbox2);
 {$ENDIF}

end;

class function TFormListb.Execute(const ACaption,AColCaption1, AColCaption2: string;
  List1, List2: TStrings): boolean;
var f:TFormListb;
begin
  f := self.Create(Application);
  try
    f.Caption := ACaption;
    f.lblColumn1.Caption := AColCaption1;
    f.lblColumn2.Caption := AColCaption2;
    f.ListBox1.Items.Assign(List1);
    f.ListBox2.Items.Assign(List2);
    Result := f.ShowModal = mrOK;
    if Result then
    begin
      List1.Assign(f.ListBox1.Items);
      List2.Assign(f.ListBox2.Items);
    end;
  finally
    f.Free;
  end;
end;

end.

