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
  StdCtrls, Buttons, ExtCtrls,
  JvBitBtn, JvLabel;

type
  TFormListb = class(TForm)
    ListBox1: TListBox;
    ListBox2: TListBox;
    Bevel1: TBevel;
    LblColumn1: TJvLabel;
    LblColumn2: TJvLabel;
    BtnLeft: TButton;
    BtnRight: TButton;
    BtnOK: TButton;
    BtnCancel: TButton;
    procedure BtnLeftClick(Sender: TObject);
    procedure BtnRightClick(Sender: TObject);
  public
    class function Execute(const ACaption, AColCaption1, AColCaption2: string;
      List1, List2: TStrings): Boolean;
  end;

implementation

{$R *.DFM}

{$IFNDEF COMPILER6_UP}

procedure CopySelected(De, Vers: TListBox);
var
  I: Integer;
begin
  for I := 0 to De.Items.Count-1 do
    if De.Selected[I] then
      Vers.Items.AddObject(De.Items[I], De.Items.Objects[I]);
  for I := De.Items.Count-1 downto 0 do
    if De.Selected[I] then
      De.Items.Delete(I);
end;

{$ENDIF}

procedure TFormListb.BtnLeftClick(Sender: TObject);
begin
  {$IFDEF COMPILER6_UP}
  ListBox2.CopySelection(ListBox1);
  while ListBox2.SelCount > 0 do
    ListBox2.DeleteSelected;
  {$ELSE}
  CopySelected(ListBox2, ListBox1);
  {$ENDIF}
end;

procedure TFormListb.BtnRightClick(Sender: TObject);
begin
  {$IFDEF COMPILER6_UP}
  ListBox1.CopySelection(ListBox2);
  while ListBox1.SelCount > 0 do
    ListBox1.DeleteSelected;
  {$ELSE}
  CopySelected(ListBox1, ListBox2);
  {$ENDIF}
end;

class function TFormListb.Execute(const ACaption, AColCaption1, AColCaption2: string;
  List1, List2: TStrings): Boolean;
var
  F: TFormListb;
begin
  F := Self.Create(Application);
  try
    F.Caption := ACaption;
    F.LblColumn1.Caption := AColCaption1;
    F.LblColumn2.Caption := AColCaption2;
    F.ListBox1.Items.Assign(List1);
    F.ListBox2.Items.Assign(List2);
    Result := F.ShowModal = mrOK;
    if Result then
    begin
      List1.Assign(F.ListBox1.Items);
      List2.Assign(F.ListBox2.Items);
    end;
  finally
    F.Free;
  end;
end;

end.

