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

unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvPluginManager, ExtCtrls, JvComponent, JvPlgIntf;

type
  TfrmMain = class(TForm, IUnknown, IMyMainAppInterface)
    JvPluginManager1: TJvPluginManager;
    Memo1: TMemo;
    Panel1: TPanel;
    butLoadPlugins: TButton;
    lstPlugins: TListBox;
    butShowPlug: TButton;
    butUnload: TButton;
    Splitter1: TSplitter;
    procedure butLoadPluginsClick(Sender: TObject);
    procedure butShowPlugClick(Sender: TObject);
    procedure JvPluginManager1ErrorLoading(Sender: TObject; s: string);
    procedure butUnloadClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }

  public
    { Public-Deklarationen }
    //
    function GetPosition : TPoint;
    procedure SetPosition(const NewPos : TPoint);
    procedure DoSomethingSpecial(Name: String; OnClick: TNotifyEvent);
  end;

var
  frmMain: TfrmMain;

implementation

uses JvPlugin;

{$R *.DFM}

procedure TfrmMain.butLoadPluginsClick(Sender: TObject);
var
  counter: integer;
  Plug: TJvPlugin;
  MyPlug: IMyPlugInInterface;
begin
  butLoadPlugins.Enabled := False;

  // Loading Plugins
  Memo1.Lines.Add('Looking in ' + ExtractFilePath(Application.Exename) + ' for Plugins');
  JvPluginManager1.LoadPlugins;
  Memo1.Lines.Add(IntToStr(JvPluginManager1.PluginCount) + ' plugins loaded.');

  // Check if plugins have correct interface
  counter := 0;
  while counter < JvPluginManager1.PluginCount do
  begin
    Plug := JvPluginManager1.Plugins[counter];
    if Plug.GetInterface(IMyPlugInInterface, MyPlug) then
    begin
      Memo1.Lines.Add(IntToStr(counter) + ' : ' + Plug.Description + ' V ' + Plug.PluginVersion);
      lstPlugins.Items.Add(Plug.Description + ' V ' + Plug.PluginVersion);

      Plug.Configure;
      MyPlug.Init(Self); // make the plugin aware of the main app
      Inc(counter);
    end
    else
    begin
      Memo1.Lines.Add('Warning : Plugin ' + Plug.Description + ' has unknown interface - unloaded!');
      JvPluginManager1.UnloadPlugin(counter);
    end;
  end;
  if (lstPlugins.ItemIndex < 0) then
    lstPlugins.ItemIndex := 0;
end;

procedure TfrmMain.butShowPlugClick(Sender: TObject);
var
  MyPlug: IMyPlugInInterface;
begin
  if (lstPlugins.ItemIndex < 0) or
    (lstPlugins.ItemIndex > lstPlugins.Items.Count) then
    Exit;

  JvPluginManager1.Plugins[lstPlugins.ItemIndex].GetInterface(IMyPlugInInterface, MyPlug);
  MyPlug.ShowPlug(self);
end;

procedure TfrmMain.JvPluginManager1ErrorLoading(Sender: TObject;
  s: string);
begin
  Memo1.Lines.Add(s);
end;

procedure TfrmMain.butUnloadClick(Sender: TObject);
begin
  while JvPluginManager1.PluginCount <> 0 do
    JvPluginManager1.UnloadPlugin(0);

  lstPlugins.Clear;

  butLoadPlugins.Enabled := True;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  butUnloadClick(nil);
end;

procedure TfrmMain.DoSomethingSpecial(Name: String; OnClick: TNotifyEvent);
begin
  ShowMessageFmt('DoSomethingSpecial was called with "%s" as the Name parameter',[Name]);
end;

function TfrmMain.GetPosition: TPoint;
begin
  Result := ClientOrigin;
end;

procedure TfrmMain.SetPosition(const NewPos: TPoint);
begin
  Top := NewPos.Y;
  Left := NewPos.X;
end;

end.

