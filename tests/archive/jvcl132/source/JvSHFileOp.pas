{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSHFileOp.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{A wrapper component for the SHFileOperation function }

unit JvSHFileOp;
{  Bugs:
    fofConfirmMouse does nothing
    fofWantMappingHandle does nothing
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,ShellAPI,JvComponent;

type
  TJvSHFileOpType=(foCopy,foDelete,foMove,foRename);
  TJvSHFileOption=(fofAllowUndo,fofConfirmMouse,fofFilesOnly,fofMultiDestFiles,
                 fofNoConfirmation,fofNoConfirmMkDir,fofRenameOnCollision,fofSilent,
                 fofSimpleProgress,fofWantMappingHandle,{fofCreateProgressDlg,}fofNoErrorUI);
  TJvSHFileOptions=set of TJvSHFileOption;

  TJvSHFileOperation = class(TJvComponent)
  private
    { Private declarations }
    FSourceFiles  :TStrings;
    FDestFiles    :TStrings;
    FOperation    :TJvSHFileOpType;
    FOptions      :TJvSHFileOptions;
    FTitle        :string;
    procedure SetSourceFIles(Value:TStrings);
    procedure SetDestFiles(Value:TStrings);
  protected
    { Protected declarations }
    function GetWinHandle:THandle;virtual;
  public
    { Public declarations }
    function Execute:boolean;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
  published
    { Published declarations }
    property SourceFiles:TStrings read FSourceFiles write SetSourceFiles;
    property DestFiles:TStrings read FDestFiles write SetDestFiles;
    property Operation:TJvSHFileOpType read FOperation write FOperation default foCopy;
    property Options:TJvSHFileOptions read FOptions write FOptions default [fofAllowUndo,fofFilesOnly];
    property Title:string read FTitle write FTitle;
  end;


implementation
{ TJvSHFileOperation }

constructor TJvSHFileOperation.Create(AOwner:TComponent);
begin
  FSourceFiles := TStringlist.Create;
  FDestFiles   := TStringlist.Create;
  FOperation := foCopy;
  FOptions := [fofAllowUndo,fofFilesOnly];
  inherited Create(AOwner);
end;

destructor TJvSHFileOperation.Destroy;
begin
  FSourceFiles.Free;
  FDestFiles.Free;
  inherited Destroy;
end;

{** returns true if no error occurred and user didn't abort }
function TJvSHFileOperation.Execute:boolean;
const
  aOperation:array[TJvSHFileOpType] of integer=(FO_COPY,FO_DELETE,FO_MOVE,FO_RENAME);
  aOption:array[TJvSHFileOption] of integer=(FOF_ALLOWUNDO,FOF_CONFIRMMOUSE,FOF_FILESONLY,FOF_MULTIDESTFILES,
                                            FOF_NOCONFIRMATION,FOF_NOCONFIRMMKDIR,FOF_RENAMEONCOLLISION,
                                            FOF_SILENT,FOF_SIMPLEPROGRESS,FOF_WANTMAPPINGHANDLE,{FOF_CREATEPROGRESSDLG,}FOF_NOERRORUI);
var
  SFOS:TShFileOpStruct;
  i:TJvSHFileOption;
  j:integer;
  ppFrom,ppTo:string;
begin
  if Length(FSourceFiles.Text) = 0 then
    Exception.Create('No files specified to TJvSHFileOperation Execute function');

  FillChar(SFOS,sizeof(TShFileOpStruct),#0);

  with SFOS do
  begin
    fAnyOperationsAborted := false;
    fFlags := 0;
    for i := Low(TJvSHFileOption) to High(TJvSHFileOption) do    // Iterate
      if i in FOptions then
        fFlags := fFlags or aOption[i];
    hNameMappings := nil; // this is never used ???
    lpszProgressTitle := PChar(FTitle);
    ppFrom := '';
    ppTo := '';
    for j := 0 to FSourceFiles.Count - 1 do
      ppFrom := ppFrom + ExpandFilename(FSourceFiles[j]) + #0;
    ppFrom := ppFrom + #0;
    pFrom := PChar(ppFrom);

    for j := 0 to FDestFiles.Count - 1 do
      ppTo := ppTo + ExpandFilename(FDestFiles[j]) + #0;
    ppTo := ppTo + #0;
    pTo := PChar(ppTo);

    wFunc := aOperation[FOperation];
    Wnd := GetWinHandle; // (Owner as TForm).Handle;
  end;    // with
  Result := SHFileOperation(SFOS) = 0;
  Result := Result and not SFOS.fAnyOperationsAborted;

  if SFOS.hNameMappings <> nil then
  begin
{    PNameMapping := Pointer(SFOS.hNameMappings);
    while PNameMapping <> nil do
    begin
      if (PNameMapping.cchOldPath <> 0) and (PNameMapping.cchNewPath <> 0) then
        FSourceFiles.Add(Format('%s=%s',[PNameMapping.pszOldPath,PNameMapping.pszNewPath]));
      Inc(PNameMapping,sizeof(TSHNameMapping));
    end;
}    
    ShFreeNameMappings(Cardinal(SFOS.hNameMappings));
  end;
end;


{ private }
procedure TJvSHFileOperation.SetSourceFiles(Value:TStrings);
begin
  FSourceFiles.Assign(Value);
end;

procedure TJvSHFileOperation.SetDestFiles(Value:TStrings);
begin
  FDestFiles.Assign(Value);
end;

function TJvSHFileOperation.GetWinHandle: THandle;
begin
  if (Owner is TWinControl) then
    Result := TWinControl(Owner).Handle
  else
    Result := GetFocus;
end;

end.
