{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExchListboxes.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvExchListboxes;

{$ObjExportAll On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvFormLists, JvBaseDlg, JvTypes ;

type
  TJvExchListboxes = class(TJvCommonDialog)
  private
    FButton1: string;
    FCaption2: string;
    FCaption: string;
    FButton2: string;
    FListbox2: TstringList;
    FListbox: TstringList;
    FTitle: string;
  protected
  public
    constructor Create(AOwner: TComponent);override;
  published
    property FirstListbox:TstringList read FListbox write FListbox;
    property SecondListbox:TstringList read FListbox2 write FListbox2;
    property FirstCaption:string read FCaption write FCaption;
    property SecondCaption:string read FCaption2 write FCaption2;
    property Button1Caption:string read FButton1 write FButton1;
    property Button2Caption:string read FButton2 write FButton2;
    property Title:string read FTitle write FTitle;
    function Execute:boolean;override;
  end;

implementation

resourcestring
   RC_Button1Caption       =     '&Add';
   RC_Button2Caption       =     '&Remove';
   RC_Column1Caption       =     'First Column';
   RC_Column2Caption       =     'Second Column';
   RC_FormCaption          =     'Listbox Editor';

{**************************************************}
constructor TJvExchListboxes.Create(AOwner: TComponent);
begin
   inherited;
   FListbox:=TstringList.Create;
   FListbox2:=TstringList.Create;
   FButton1:=RC_Button1Caption;
   FButton2:=RC_Button2Caption;
   FCaption:=RC_Column1Caption;
   FCaption2:=RC_Column2Caption;
   FTitle:=RC_FormCaption;
end;
{**************************************************}
function TJvExchListboxes.Execute: boolean;
begin
   with TFormListb.Create(Application) do
   begin
      buBitBtn1.Caption:=FButton1;
      buBitBtn2.Caption:=FButton2;
      buLabel1.caption:=FCaption;
      buLabel2.caption:=FCaption2;
      Listbox1.items:=FListbox;
      Listbox2.items:=FListbox2;
      Caption:=FTitle;
      showmodal;
      if tag=1 then
      begin
         result:=true;
         FListbox.Assign(TstringList(ListBox1.Items));
         FListbox2.Assign(TstringList(Listbox2.items));
      end
      else result:=false;
      free;
   end;
end;
{**************************************************}
end.
