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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvFormLists;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, JvBitBtn, JvLabel;

type
  TFormListb = class(TForm)
    ListBox1: TListBox;
    ListBox2: TListBox;
    BUBitBtn1: TJvBitBtn;
    BUBitBtn2: TJvBitBtn;
    Bevel1: TBevel;
    BULabel1: TJvLabel;
    BULabel2: TJvLabel;
    BUBitBtn3: TJvBitBtn;
    BUBitBtn4: TJvBitBtn;
    procedure BUButton1Click(Sender: TObject);
    procedure BUButton2Click(Sender: TObject);
    procedure BUBitBtn1Click(Sender: TObject);
    procedure BUBitBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormListb: TFormListb;

implementation

{$R *.DFM}

{************************************************************}
procedure TFormListb.BUButton1Click(Sender: TObject);
begin
   self.tag:=1;
   self.close;
end;
{************************************************************}
procedure TFormListb.BUButton2Click(Sender: TObject);
begin
   self.tag:=0;
   self.close;
end;
{************************************************************}
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
{************************************************************}
procedure TFormListb.BUBitBtn1Click(Sender: TObject);
begin
   //add (from 1 to 2)
   CopySelected(self.listbox2,self.listbox1);
end;
{************************************************************}
procedure TFormListb.BUBitBtn2Click(Sender: TObject);
begin
   //remove
   CopySelected(self.listbox1,self.listbox2);
end;
{************************************************************}
end.
