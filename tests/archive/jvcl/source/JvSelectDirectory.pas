{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSelectDirectory.PAS, released on 2001-02-28.

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
{$I WINDOWSONLY.INC}

unit JvSelectDirectory;

interface

uses
  Classes, FileCtrl,
  JvBaseDlg;

type
  { TODO -opeter3 : Rewrite to not depend on FileCtrl? }
  TJvSelectDirectory = class(TJvCommonDialog)
  private
    FDirectory: string;
    FClassicDialog: Boolean;
    FHelpContext: Longint;
    FInitialDir: string;
    FOptions: TSelectDirOpts;
    FTitle: string;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ClassicDialog: Boolean read FClassicDialog write FClassicDialog default True;
    property Directory: string read FDirectory;
    property HelpContext: Longint read FHelpContext write FHelpContext default 0;
    property InitialDir: string read FInitialDir write FInitialDir;
    property Options: TSelectDirOpts read FOptions write FOptions default [sdAllowCreate, sdPerformCreate, sdPrompt];
    property Title: string read FTitle write FTitle;
    function Execute: Boolean; override;
  end;

implementation

constructor TJvSelectDirectory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClassicDialog := True;
  FDirectory := '';
  FInitialDir := '';
  FHelpContext := 0;
  FOptions := [sdAllowCreate, sdPerformCreate, sdPrompt];
  FTitle := '';
end;

function TJvSelectDirectory.Execute: Boolean;
begin
  FDirectory := InitialDir;
  if ClassicDialog then
    Result := SelectDirectory(FDirectory, Options, HelpContext)
  else
    Result := SelectDirectory(Title, InitialDir, FDirectory);
end;

end.

