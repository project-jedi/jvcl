{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormLog.PAS, released on 2001-02-28.

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

unit JvFormLog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ActnList, ImgList, JvListView, JvPrint, JvComponent;

type
  TfoLog = class(TForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ImageList1: TImageList;
    ActionList1: TActionList;
    Save: TAction;
    Print: TAction;
    BUListView1: TJvListView;
    SaveDialog1: TSaveDialog;
    BUPrint1: TJvPrint;
    procedure SaveExecute(Sender: TObject);
    procedure PrintExecute(Sender: TObject);
  private
  public
  end;

implementation

{$R *.DFM}

{*******************************************************}
procedure TfoLog.SaveExecute(Sender: TObject);
begin
  if not(SaveDialog1.Execute) then
    Exit;
  if SaveDialog1.FilterIndex = 1 then
    BUListView1.SaveToCSV(SaveDialog1.FileName)
  else
    BUListView1.SaveToFile(SaveDialog1.FileName)
end;
{*******************************************************}
procedure TfoLog.PrintExecute(Sender: TObject);
var
 i: Integer;
 ts: TStringList;
begin
  ts := TStringList.Create;
  with ts do
  begin
    for i:=0 to BUListView1.Items.Count-1 do
      with BUListView1.Items[i] do
       Add('['+Caption+']'+SubItems[0]+' > '+SubItems[1]);
    BUPrint1.Print(ts);
    Free;
  end;
end;
{*******************************************************}
end.
