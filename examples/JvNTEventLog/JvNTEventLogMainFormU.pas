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

unit JvNTEventLogMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, JvNTEventLog, JvComponent;
const
  CM_CHECKOSVERSION = WM_USER + 1;

type
  TJvNTEventLogMainForm = class(TForm)
    btnRefresh: TButton;
    ListBox1: TListBox;
    ButtonsPanel: TPanel;
    Splitter1: TSplitter;
    ListView1: TListView;
    JvNTEventLog1: TJvNTEventLog;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure CMCHECKOSVERSION(var Msg:TMEssage); message CM_CHECKOSVERSION;
  private
    { Private declarations }
    procedure ReadEvents;
  end;

var
  JvNTEventLogMainForm: TJvNTEventLogMainForm;

implementation

{$R *.dfm}

procedure TJvNTEventLogMainForm.CMCHECKOSVERSION(var Msg: TMEssage);
begin
//  if Win32Platform <> VER_PLATFORM_WIN32_NT	then
  ShowMessage('This demo only works on NT based OS''s (NT 4, Win2k and WinXP).');
end;

procedure TJvNTEventLogMainForm.FormCreate(Sender: TObject);
begin
  JvNTEventLog1.ReadEventLogs(ListBox1.Items);
  JvNTEventLog1.Active := True;
  PostMessage(Handle, CM_CHECKOSVERSION,0,0);
end;

procedure TJvNTEventLogMainForm.ListBox1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex > -1 then
  begin
    JvNTEventLog1.Active := false;
    JvNTEventLog1.Log := ListBox1.Items[ListBox1.ItemIndex];
    JvNTEventLog1.Source := ListBox1.Items[ListBox1.ItemIndex];
    JvNTEventLog1.Active := True;
    ReadEvents;
  end;
end;

procedure TJvNTEventLogMainForm.ReadEvents;
var
  item: TListItem;
begin
  ListView1.Items.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    ListView1.Items.Clear;

    JvNTEventLog1.First;
    while not JvNTEventLog1.Eof do
    begin
      item := ListView1.Items.Add;

      item.Caption := JvNTEventLog1.EventRecord.EventType;
      item.SubItems.Add(DateToStr(JvNTEventLog1.EventRecord.DateTime));
      item.SubItems.Add(TimeToStr(JvNTEventLog1.EventRecord.DateTime));
      item.SubItems.Add(JvNTEventLog1.EventRecord.Source);
      item.SubItems.Add(IntToStr(JvNTEventLog1.EventRecord.Category));
      item.SubItems.Add(IntToStr(JvNTEventLog1.EventRecord.ID and $0FFFFFFF));
      item.SubItems.Add(JvNTEventLog1.EventRecord.Username);
      item.SubItems.Add(JvNTEventLog1.EventRecord.Computer);
      JvNTEventLog1.Next;
    end;
  finally
    ListView1.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

end.

