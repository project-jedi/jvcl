{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormFind.PAS, released on 2001-02-28.

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

unit JvFormFind;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, PackTools, Dsgnintf, ExptIntf, ToolIntf, ExtCtrls,
  ComCtrls, JvPcx, JvButton;

type
  TFormFin = class(TForm)
    Image2: TImage;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    StaticText7: TStaticText;
    TabSheet2: TTabSheet;
    BUButton4: TJvButton;
    BUButton5: TJvButton;
    BUButton6: TJvButton;
    RadioGroup1: TRadioGroup;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    GroupBox4: TGroupBox;
    CheckBox8: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox2: TCheckBox;
    StaticText1: TStaticText;
    GroupBox5: TGroupBox;
    Edit5: TEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    procedure BUButton1Click(Sender: TObject);
    procedure BUButton2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BUButton5Click(Sender: TObject);
    procedure BUButton6Click(Sender: TObject);
  private
    function GetOptions: string;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormFin: TFormFin;

implementation

{$R *.DFM}

{************************************************************}
procedure TFormFin.BUButton1Click(Sender: TObject);
begin
   self.close;
end;
{************************************************************}
function TFormFin.GetOptions:string;
begin
   result:='';
   if self.checkbox2.checked then result:=result+'+faReadOnly';
   if self.checkbox3.checked then result:=result+'+faHidden';
   if self.checkbox4.checked then result:=result+'+faSysFile';
   if self.checkbox5.checked then result:=result+'+faVolumeID';
   if self.checkbox6.checked then result:=result+'+faDirectory';
   if self.checkbox7.checked then result:=result+'+faArchive';
   if self.checkbox8.checked then result:=result+'+faAnyFile';
   if result<>'' then result:=Copy(result,2,length(result));
end;
{************************************************************}
procedure TFormFin.BUButton2Click(Sender: TObject);
var
   ts:TstringList;
begin
   //Generate
   ts:=TstringList.Create;

   //verify options
   if (self.checkbox2.checked=false) and (self.checkbox4.checked=false)
      and (self.checkbox5.checked=false) and (self.checkbox6.checked=false)
      and (self.checkbox7.checked=false) and (self.checkbox8.checked=false) then
      begin
         beep;
         exit;
      end;


   if self.RadioGroup1.ItemIndex=0 then
   begin
      //in a procedure
      ts.add('{************************************************************}');
      ts.add('procedure '+self.edit1.text+'(path:string);');
      ts.add('var');
      ts.add('   t:TSearchRec;');
      ts.add('   res:Integer;');
      ts.add('begin');
      ts.add('     if (path<>'''')and(path[length(path)]<>''\'') then path:=path+''\'';');
      ts.add('     res:=findfirst(path+'''+self.edit5.text+''','+GetOptions+',t);');
      ts.add('     while res=0 do');
      ts.add('     begin');
      ts.add('          if (t.name<>''.'') and (t.name<>''..'') then ');
      ts.add('          begin');
      if self.checkbox1.checked then
      begin
         ts.add('               if DirectoryExists(path+t.name) then //include FileCtrl for this function');
         ts.add('                 '+self.Edit1.text+'(path+t.name)');
         ts.add('               else');
         ts.add('                 //place your code here !');
         ts.add('                 ShowMessage(t.name); //replace by your code !');
      end
      else
      begin
         ts.add('               //place your code here !');
         ts.add('               ShowMessage(t.name); //replace by your code !');
      end;
      ts.add('          end;');
      ts.add('          res:=FindNext(t);');
      ts.add('     end;');
      ts.add('     findclose(t);');
      ts.add('end;');
      ts.add('{************************************************************}');
   end
   else
   begin
      //in current code

      ts.add('     if ('+self.Edit2.text+'<>'''')and('+self.edit2.text+'[length('+self.Edit2.text+')]<>''\'') then '+self.edit2.text+':='+self.Edit2.text+'+''\'';');
      ts.add('     '+self.Edit4.text+':=findfirst('+self.edit2.text+'+'''+self.edit5.text+''','+GetOptions+','+self.edit3.text+');');
      ts.add('     while '+self.edit4.text+'=0 do');
      ts.add('     begin');
      ts.add('          if ('+self.edit3.text+'.name<>''.'') and ('+self.edit3.text+'.name<>''..'') then ');
      ts.add('          begin');
      ts.add('               //place your code here !');
      ts.add('               ShowMessage('+self.edit3.text+'.name); //replace by your code !');
      ts.add('          end;');
      ts.add('          '+self.Edit4.text+':=FindNext('+self.Edit3.text+');');
      ts.add('     end;');
      ts.add('     findclose('+self.Edit3.text+');');
   end;
   with TJvCodeEditor.Create(ToolServices.GetCurrentFile) do
   begin
      GoToCurrentPosition;
      Addstring(ts.Text);
      free;
   end;
   ts.free;
   self.close;
end;
{************************************************************}
procedure TFormFin.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if ord(key)=VK_ESCAPE then self.BUButton1Click(Sender);
end;
{************************************************************}
procedure TFormFin.BUButton5Click(Sender: TObject);
begin
   //next
   if self.PageControl1.ActivePage=self.tabsheet1 then
   begin
      self.PageControl1.activepage:=self.tabsheet2;
      self.bubutton6.enabled:=true;
   end
   else if self.PageControl1.ActivePage=self.tabsheet2 then
   begin
      self.BUButton5.Caption:='&Ok';
      if self.RadioGroup1.ItemIndex=0 then
         self.PageControl1.activepage:=self.tabsheet3
      else
         self.PageControl1.activepage:=self.tabsheet4;
   end
   else if (self.pagecontrol1.activepage=self.tabsheet3) or (self.pagecontrol1.activepage=self.tabsheet4) then
        self.BUButton2Click(Sender);
end;
{************************************************************}
procedure TFormFin.BUButton6Click(Sender: TObject);
begin
   //previous
   self.BUButton5.Caption:='&Next';
   if self.PageControl1.ActivePage=self.tabsheet4 then
      self.pagecontrol1.activepage:=self.tabsheet2
   else if self.PageControl1.ActivePage=self.tabsheet3 then
      self.pagecontrol1.activepage:=self.tabsheet2
   else if self.PageControl1.ActivePage=self.tabsheet2 then
   begin
      self.pagecontrol1.activepage:=self.tabsheet1;
      self.BUButton6.Enabled:=false;
   end;
end;
{************************************************************}
end.
