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

unit JvShellHookDemoMainFormU;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF COMPILER6_UP}Variants, {$ENDIF}Classes,
  Graphics, Controls, Forms, Dialogs, JvShellHook, StdCtrls, ComCtrls;

type
  TJvShellHookDemoMainForm = class(TForm)
    btnClear: TButton;
    Label1: TLabel;
    chkActive: TCheckBox;
    chkNoRedraw: TCheckBox;
    lvMessages: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure chkActiveClick(Sender: TObject);
    procedure lvMessagesResize(Sender: TObject);
  private
    procedure DoShellMessage(Sender: TObject; var Message: TMessage);
  public
    SH: TJvShellHook;
  end;

var
  JvShellHookDemoMainForm: TJvShellHookDemoMainForm;

implementation

{$R *.dfm}

procedure TJvShellHookDemoMainForm.FormCreate(Sender: TObject);
begin
  SH := TJvShellHook.Create(Self);
  SH.OnShellMessage := DoShellMessage;
  chkActive.Enabled := InitJvShellHooks;
  {$IFDEF COMPILER6_UP}
    lvMessages.BorderStyle := bsNone;
    lvMessages.BevelKind := bkFlat;
    lvMessages.BevelInner := bvNone;
  {$ELSE}
    lvMessages.BorderStyle := bsSingle;
  {$ENDIF}
end;

procedure TJvShellHookDemoMainForm.FormDestroy(Sender: TObject);
begin
  SH.Free;
end;

procedure TJvShellHookDemoMainForm.btnClearClick(Sender: TObject);
begin
  lvMessages.Items.Clear;
end;

function GetAppCommand(lParam: integer): string;
var tmp: string;
begin
  case GET_APPCOMMAND_LPARAM(lParam) of
    APPCOMMAND_BROWSER_BACKWARD: tmp := 'APPCOMMAND_BROWSER_BACKWARD';
    APPCOMMAND_BROWSER_FORWARD: tmp := 'APPCOMMAND_BROWSER_FORWARD';
    APPCOMMAND_BROWSER_REFRESH: tmp := 'APPCOMMAND_BROWSER_REFRESH';
    APPCOMMAND_BROWSER_STOP: tmp := 'APPCOMMAND_BROWSER_STOP';
    APPCOMMAND_BROWSER_SEARCH: tmp := 'APPCOMMAND_BROWSER_SEARCH';
    APPCOMMAND_BROWSER_FAVORITES: tmp := 'APPCOMMAND_BROWSER_FAVORITES';
    APPCOMMAND_BROWSER_HOME: tmp := 'APPCOMMAND_BROWSER_HOME';
    APPCOMMAND_VOLUME_MUTE: tmp := 'APPCOMMAND_VOLUME_MUTE';
    APPCOMMAND_VOLUME_DOWN: tmp := 'APPCOMMAND_VOLUME_DOWN';
    APPCOMMAND_VOLUME_UP: tmp := 'APPCOMMAND_VOLUME_UP';
    APPCOMMAND_MEDIA_NEXTTRACK: tmp := 'APPCOMMAND_MEDIA_NEXTTRACK';
    APPCOMMAND_MEDIA_PREVIOUSTRACK: tmp := 'APPCOMMAND_MEDIA_PREVIOUSTRACK';
    APPCOMMAND_MEDIA_STOP: tmp := 'APPCOMMAND_MEDIA_STOP';
    APPCOMMAND_MEDIA_PLAY_PAUSE: tmp := 'APPCOMMAND_MEDIA_PLAY_PAUSE';
    APPCOMMAND_LAUNCH_MAIL: tmp := 'APPCOMMAND_LAUNCH_MAIL';
    APPCOMMAND_LAUNCH_MEDIA_SELECT: tmp := 'APPCOMMAND_LAUNCH_MEDIA_SELECT';
    APPCOMMAND_LAUNCH_APP1: tmp := 'APPCOMMAND_LAUNCH_APP1';
    APPCOMMAND_LAUNCH_APP2: tmp := 'APPCOMMAND_LAUNCH_APP2';
    APPCOMMAND_BASS_DOWN: tmp := 'APPCOMMAND_BASS_DOWN';
    APPCOMMAND_BASS_BOOST: tmp := 'APPCOMMAND_BASS_BOOST';
    APPCOMMAND_BASS_UP: tmp := 'APPCOMMAND_BASS_UP';
    APPCOMMAND_TREBLE_DOWN: tmp := 'APPCOMMAND_TREBLE_DOWN';
    APPCOMMAND_TREBLE_UP: tmp := 'APPCOMMAND_TREBLE_UP';
    APPCOMMAND_MICROPHONE_VOLUME_MUTE: tmp := 'APPCOMMAND_MICROPHONE_VOLUME_MUTE';
    APPCOMMAND_MICROPHONE_VOLUME_DOWN: tmp := 'APPCOMMAND_MICROPHONE_VOLUME_DOWN';
    APPCOMMAND_MICROPHONE_VOLUME_UP: tmp := 'APPCOMMAND_MICROPHONE_VOLUME_UP';
    APPCOMMAND_HELP: tmp := 'APPCOMMAND_HELP';
    APPCOMMAND_FIND: tmp := 'APPCOMMAND_FIND';
    APPCOMMAND_NEW: tmp := 'APPCOMMAND_NEW';
    APPCOMMAND_OPEN: tmp := 'APPCOMMAND_OPEN';
    APPCOMMAND_CLOSE: tmp := 'APPCOMMAND_CLOSE';
    APPCOMMAND_SAVE: tmp := 'APPCOMMAND_SAVE';
    APPCOMMAND_PRINT: tmp := 'APPCOMMAND_PRINT';
    APPCOMMAND_UNDO: tmp := 'APPCOMMAND_UNDO';
    APPCOMMAND_REDO: tmp := 'APPCOMMAND_REDO';
    APPCOMMAND_COPY: tmp := 'APPCOMMAND_COPY';
    APPCOMMAND_CUT: tmp := 'APPCOMMAND_CUT';
    APPCOMMAND_PASTE: tmp := 'APPCOMMAND_PASTE';
    APPCOMMAND_REPLY_TO_MAIL: tmp := 'APPCOMMAND_REPLY_TO_MAIL';
    APPCOMMAND_FORWARD_MAIL: tmp := 'APPCOMMAND_FORWARD_MAIL';
    APPCOMMAND_SEND_MAIL: tmp := 'APPCOMMAND_SEND_MAIL';
    APPCOMMAND_SPELL_CHECK: tmp := 'APPCOMMAND_SPELL_CHECK';
    APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE: tmp := 'APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE';
    APPCOMMAND_MIC_ON_OFF_TOGGLE: tmp := 'APPCOMMAND_MIC_ON_OFF_TOGGLE';
    APPCOMMAND_CORRECTION_LIST: tmp := 'APPCOMMAND_CORRECTION_LIST';
    APPCOMMAND_MEDIA_PLAY: tmp := 'APPCOMMAND_MEDIA_PLAY';
    APPCOMMAND_MEDIA_PAUSE: tmp := 'APPCOMMAND_MEDIA_PAUSE';
    APPCOMMAND_MEDIA_RECORD: tmp := 'APPCOMMAND_MEDIA_RECORD';
    APPCOMMAND_MEDIA_FAST_FORWARD: tmp := 'APPCOMMAND_MEDIA_FAST_FORWARD';
    APPCOMMAND_MEDIA_REWIND: tmp := 'APPCOMMAND_MEDIA_REWIND';
    APPCOMMAND_MEDIA_CHANNEL_UP: tmp := 'APPCOMMAND_MEDIA_CHANNEL_UP';
    APPCOMMAND_MEDIA_CHANNEL_DOWN: tmp := 'APPCOMMAND_MEDIA_CHANNEL_DOWN';
  else
    tmp := Format('Unknown ($%.8x)', [lParam]);
  end;
  Result := Format('HSHELL_APPCOMMAND: %s', [tmp]);
end;

procedure TJvShellHookDemoMainForm.DoShellMessage(Sender: TObject; var Message: TMessage);
var S: string;
begin
  with Message do
  begin
    case wParam of
      HSHELL_WINDOWCREATED: S := 'HSHELL_WINDOWCREATED';
      HSHELL_WINDOWDESTROYED: S := 'HSHELL_WINDOWDESTROYED';
      HSHELL_ACTIVATESHELLWINDOW: S := 'HSHELL_ACTIVATESHELLWINDOW';
      HSHELL_WINDOWACTIVATED:
      begin
        S := 'HSHELL_WINDOWACTIVATED';
        if HWND(lParam) = Application.Handle then
          S := S + ' (this)';
      end;
      HSHELL_GETMINRECT: S := 'HSHELL_GETMINRECT';
      HSHELL_REDRAW:
        if chkNoRedraw.Checked then Exit else S := 'HSHELL_REDRAW';
      HSHELL_TASKMAN: S := 'HSHELL_TASKMAN';
      HSHELL_LANGUAGE: S := 'HSHELL_LANGUAGE';
      HSHELL_SYSMENU: S := 'HSHELL_SYSMENU';
      HSHELL_ENDTASK: S := 'HSHELL_ENDTASK';
      HSHELL_ACCESSIBILITYSTATE: S := 'HSHELL_ACCESSIBILITYSTATE';
      HSHELL_WINDOWREPLACED: S := 'HSHELL_WINDOWREPLACED';
      HSHELL_WINDOWREPLACING: S := 'HSHELL_WINDOWREPLACING';
      HSHELL_FLASH: S := 'HSHELL_FLASH';
      HSHELL_RUDEAPPACTIVATED: S := 'HSHELL_RUDEAPPACTIVATED';
      HSHELL_APPCOMMAND: S := GetAppCommand(lParam);
    else
      S := Format('Unknown command ($%.8x)', [wParam]);
    end;
    with lvMessages.Items.Add do
    begin
      Caption := S;
      SubItems.Add(IntToStr(wParam));
      SubItems.Add(IntToStr(lParam));
      SubItems.Add(IntToStr(Result));
      MakeVisible(false);
      Selected := true;
      Focused := true;
    end;
  end;
end;

procedure TJvShellHookDemoMainForm.chkActiveClick(Sender: TObject);
begin
  SH.Active := chkActive.Checked;
end;                 
                         
procedure TJvShellHookDemoMainForm.lvMessagesResize(Sender: TObject);
begin
  lvMessages.Columns[3].Width := -2;
end;

end.

