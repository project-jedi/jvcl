{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRecentDocs.PAS, released on 2001-02-28.

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

unit JvRecentDocs;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ShlObj, JvDirectories, JvTypes, JvFileInformations,
  JvComponent;

type
  TJvRecentDocs = class(TJvComponent)
  private
    FDirs: TJvDirectories;
    FLink: TJvFileInformations;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    function GetDocs: TStringList;
    procedure AddToDocuments(Value: string);
  end;

implementation

{*****************************************************}

constructor TJvRecentDocs.Create(AOwner: TComponent);
begin
  inherited;
  FDirs := TJvDirectories.Create(Self);
  Flink := TJvFileInformations.Create(Self);
end;

{*****************************************************}

destructor TJvRecentDocs.Destroy;
begin
  FDirs.Free;
  FLink.Free;
  inherited;
end;

{*****************************************************}

function TJvRecentDocs.GetDocs: TStringList;
var
  path: string;
  t: TSearchRec;
  res: Integer;
begin
  Result := TStringList.Create;
  Result.Clear;

  path := FDirs.Recent + '\';
  //search for all files
  res := FindFirst(path + '*.*', faAnyFile, t);
  while res = 0 do
  begin
    if (t.Name <> '.') and (t.Name <> '..') then
      Result.Add(path + T.Name);
    res := FindNext(t);
  end;
  FindClose(t);
end;

{*****************************************************}

procedure TJvRecentDocs.AddToDocuments(Value: string);
begin
  SHAddToRecentDocs(SHARD_PATH, PChar(Value));
end;

end.
