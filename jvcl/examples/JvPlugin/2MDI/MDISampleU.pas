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

unit MDISampleU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, ComCtrls, Buttons, JvPluginManager, JvComponent;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Window1: TMenuItem;
    Exit1: TMenuItem;
    panToolbar: TPanel;
    uilPluginManager1: TJvPluginManager;
    StatusBar1: TStatusBar;
    Commands1: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure uilPluginManager1AfterLoading(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure uilPluginManager1NewCommand(Sender: TObject; ACaption, AHint,
      AData: String; AShortCut: TShortCut; ABitmap: TBitmap;
      AEvent: TNotifyEvent);
  private
    { Private declarations }
       NumButtons : integer;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Exit1Click(Sender: TObject);
begin
   Close;
end;

procedure TForm1.uilPluginManager1AfterLoading(Sender: TObject);
begin
   Statusbar1.SimpleText := 'Plugins Loaded: ' + IntToStr(uilPluginManager1.PluginCount);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
   uilPluginManager1.LoadPlugins;
end;


procedure TForm1.uilPluginManager1NewCommand(Sender: TObject; ACaption,
  AHint, AData: String; AShortCut: TShortCut; ABitmap: TBitmap;
  AEvent: TNotifyEvent);
var
   Item : TMenuItem;
begin
   Item := NewItem(ACaption, scNone, False, True, AEvent, 0, '');
   MainMenu1.Items[1].Add(Item);
   with TSpeedButton.Create(panToolbar) do
   begin
      Top := 4;
      Left := 4+(NumButtons * Width);
      Parent := panToolbar;
      Hint := AHint;
      if ABitmap <> nil then
         Glyph.Handle := ABitmap.Handle;
      try
         NumGlyphs := Glyph.Width div Glyph.Height;
      except

      end;
      OnClick := AEvent;
   end;    // with
   Inc(NumButtons);
end;

end.

