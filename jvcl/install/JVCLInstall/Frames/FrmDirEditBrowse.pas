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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

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
    BtnJCLDirBrowse: TButton;
    procedure BtnJCLDirBrowseClick(Sender: TObject);
  private
    FOnChange: TDirEditChangeEvent;
    FUserData: TObject;
  public
    class function Build(const Caption, Dir: string; OnChange: TDirEditChangeEvent;
      UserData: TObject; Client: TWinControl): TFrameDirEditBrowse;
  published
    property OnChange: TDirEditChangeEvent read FOnChange write FOnChange;
  end;

implementation

uses
  AHCompBrowseFolderDlg;

{$R *.dfm}

{ TFrameDirEditBrowse }

class function TFrameDirEditBrowse.Build(const Caption, Dir: string;
  OnChange: TDirEditChangeEvent; UserData: TObject;
  Client: TWinControl): TFrameDirEditBrowse;
begin
  Result := TFrameDirEditBrowse.Create(Client);
  Result.Name := '';
  Result.LblCaption.Caption := Caption;
  Result.FOnChange := OnChange;
  Result.FUserData := UserData;
  Result.Parent := Client;
  Result.Height := 50;
  Result.EditDirectory.Text := Dir;
end;


procedure TFrameDirEditBrowse.BtnJCLDirBrowseClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := EditDirectory.Text;
  if BrowseDirectory(Dir, 'Select the directory where the JCL source is.', 0) then
  begin
    if Assigned(FOnChange) then
      FOnChange(Self, FUserData, Dir);
    if Dir <> '' then
      EditDirectory.Text := Dir;
  end;
end;

end.
