{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRunDlg.PAS, released on 2001-02-28.

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

unit JvRunDlg;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvBaseDlg, JvHDialogs2;

type
  TJvRunOption = (roNoBrowse, roNoDefault, roCalcDirectory, roNoLabel, roNoSeparateMem);
  TJvRunOptions = set of TJvRunOption;

  TJvRunDlg = class(TJvCommonDialogP)
  private
    FDescription: string;
    FDirectory: string;
    FIcon: TIcon;
    FOptions: TJvRunOptions;
    FTitle: string;
    procedure SetIcon(Value: TIcon);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Description: string read FDescription write FDescription;
    property Directory: string read FDirectory write FDirectory;
    property Icon: TIcon read FIcon write SetIcon;
    property Options: TJvRunOptions read FOptions write FOptions default [];
    property Title: string read FTitle write FTitle;
    procedure Execute; override;
  end;

implementation

{**************************************************}

constructor TJvRunDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIcon := (AOwner as TForm).Icon;
  FOptions := [];
end;

{**************************************************}

procedure TJvRunDlg.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
end;

{**************************************************}

procedure TJvRunDlg.Execute;
const
  OptionsMapping: array[TJvRunOption] of Longint =
  (RFF_NOBROWSE, RFF_NODEFAULT, RFF_CALCDIRECTORY,
    RFF_NOLABEL, RFF_NOSEPARATEMEM);
var
  Opt: Longint;
  I: TJvRunOption;

  function PCharOrNil(const AString: string): PChar;
  begin
    if AString = '' then
      Result := nil
    else
      Result := PChar(AString);
  end;

begin
  Opt := 0;
  for I := Low(TJvRunOption) to High(TJvRunOption) do
    if I in Options then
      Opt := Opt or OptionsMapping[I];
  SHRunFileDlg(0, Application.Icon.Handle, PCharOrNil(Directory),
    PCharOrNil(Title), PCharOrNil(Description), Opt);
end;

end.
