{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FrmDirEditBrowse.pas, released on 2004-03-29.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit FrmDirEditBrowse;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  { TDirEditChangeEvent: set Dir to '' if you do not want to change the
    directory. }
  TDirEditChangeEvent = procedure(Sender: TObject; UserData: TObject;
    var Dir: string) of object;

  TFrameDirEditBrowse = class(TFrame)
    LblCaption: TLabel;
    Bevel: TBevel;
    EditDirectory: TEdit;
    BtnDirBrowse: TButton;
    procedure BtnDirBrowseClick(Sender: TObject);
    procedure EditDirectoryEnter(Sender: TObject);
    procedure EditDirectoryClick(Sender: TObject);
  private
    FOnChange: TDirEditChangeEvent;
    FUserData: TObject;
    FAllowEmpty: Boolean;
    FEmpty: Boolean;
    procedure DoCustomize(Sender: TObject; Handle: HWND);
    procedure DoCommand(Sender: TObject; var Msg: TWMCommand; var Handled: Boolean);
    function BrowseDirectory(var AFolderName: string): Boolean;
  public
    class function Build(const Caption, Dir: string; OnChange: TDirEditChangeEvent;
      UserData: TObject; Client: TWinControl; const AButtonHint: string): TFrameDirEditBrowse;
  published
    property OnChange: TDirEditChangeEvent read FOnChange write FOnChange;
    property AllowEmpty: Boolean read FAllowEmpty write FAllowEmpty;
  end;

implementation

uses
  InstallerConsts, AHCompBrowseFolderDlg, Core;

{$R *.dfm}

{ TFrameDirEditBrowse }

const
  NoDirectoryButtonId = $FFFF;

class function TFrameDirEditBrowse.Build(const Caption, Dir: string;
  OnChange: TDirEditChangeEvent; UserData: TObject;
  Client: TWinControl; const AButtonHint: string): TFrameDirEditBrowse;
begin
  Result := TFrameDirEditBrowse.Create(Client);
  PackageInstaller.Translate(Result);
  Result.Name := '';
  Result.LblCaption.Caption := Caption;
  Result.FOnChange := OnChange;
  Result.FUserData := UserData;
  Result.Parent := Client;
  Result.Height := 50;
  Result.EditDirectory.Text := Dir;
  Result.BtnDirBrowse.Hint := AButtonHint;
end;

procedure TFrameDirEditBrowse.BtnDirBrowseClick(Sender: TObject);
var
  Dir: string;
begin
  if BtnDirBrowse.Tag <> 0 then
    Exit; // no multiple execution
  BtnDirBrowse.Tag := 1;
  try
    Dir := EditDirectory.Text;
    if BrowseDirectory(Dir) then
    begin
      if Assigned(FOnChange) then
        FOnChange(Self, FUserData, Dir);
      if (Dir <> '') or AllowEmpty then
        EditDirectory.Text := Dir;
    end;
  finally
    BtnDirBrowse.Tag := 0;
  end;
end;

function TFrameDirEditBrowse.BrowseDirectory(var AFolderName: string): Boolean;
begin
  FEmpty := False;
  with TBrowseFolderDialog.Create(Application) do
  try
    DialogText := RsSelectDir;
    FolderName := AFolderName;
    OnCustomize := DoCustomize;
    OnCommand := DoCommand;
    Result := Execute;
    if Result then
      AFolderName := FolderName;
    if FEmpty and AllowEmpty then
    begin
      AFolderName := '';
      Result := True;
    end;
  finally
    Free;
  end;
end;

procedure TFrameDirEditBrowse.DoCustomize(Sender: TObject; Handle: HWND);
const
  SBtn = 'BUTTON'; // do not localize
var
  BtnHandle, HelpBtn, BtnFont: THandle;
  BtnRect: TRect;
  CaptionSize: TSize;
  BtnCaption: string;
  DC: HDC;
begin
  if AllowEmpty then
  begin
    BtnHandle := FindWindowEx(Handle, 0, SBtn, nil);
    if (BtnHandle <> 0) then
    begin
      // We need the witdh of the text at runtime because it gets localized
      BtnCaption := RsNoDirectoryButton;
      DC := GetWindowDC(BtnHandle);
      GetTextExtentPoint32(DC, PChar(BtnCaption), Length(BtnCaption), CaptionSize);
      ReleaseDC(BtnHandle, DC);

      // And we need the BtnRect to get the right top and height for the button
      GetWindowRect(BtnHandle, BtnRect);
      Windows.ScreenToClient(Handle, BtnRect.TopLeft);
      Windows.ScreenToClient(Handle, BtnRect.BottomRight);
      
      BtnFont := SendMessage(Handle, WM_GETFONT, 0, 0);

      // Create the button
      HelpBtn := CreateWindow(SBtn, PChar(BtnCaption),
        WS_CHILD or WS_CLIPSIBLINGS or WS_VISIBLE or BS_PUSHBUTTON or WS_TABSTOP,
        12, BtnRect.Top, CaptionSize.cx, BtnRect.Bottom-BtnRect.Top,
        Handle, NoDirectoryButtonId, HInstance, nil);
        
      if BtnFont <> 0 then
        SendMessage(HelpBtn, WM_SETFONT, BtnFont, MakeLParam(1, 0));
      UpdateWindow(Handle);
    end;
  end;
end;

procedure TFrameDirEditBrowse.DoCommand(Sender: TObject;
  var Msg: TWMCommand; var Handled: Boolean);
begin
  if (Msg.ItemID = NoDirectoryButtonId) and
     (Msg.NotifyCode = BN_CLICKED) and AllowEmpty then
  begin
    EditDirectory.Text := '';
    FEmpty := True;
    Handled := True;
    PostMessage(TBrowseFolderDialog(Sender).Handle, WM_CLOSE, 0, 0);
  end;
end;

procedure TFrameDirEditBrowse.EditDirectoryEnter(Sender: TObject);
begin
  BtnDirBrowse.Click;
end;

procedure TFrameDirEditBrowse.EditDirectoryClick(Sender: TObject);
begin
  BtnDirBrowse.Click;
end;

end.
