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

unit DropFrm;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImgList;

type
  TDropFrmAcceptEvent = procedure(Sender: TObject; Index: integer; const Value: string) of object;
  TfrmDrop = class(TForm)
    Label1: TLabel;
    btnCancel: TButton;
    tvFolders: TTreeView;
    ilSmallIcons: TImageList;
    btnOK: TButton;
    PathLabel: TLabel;
    procedure tvFoldersDblClick(Sender: TObject);
    procedure tvFoldersExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure tvFoldersGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure tvFoldersGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvFoldersChange(Sender: TObject; Node: TTreeNode);
  private
    FOnAccept: TDropFrmAcceptEvent;
    FIncludeFiles: boolean;
    procedure BuildFolderList(Items: TTreeNodes; Parent: TTreeNode; const Root: string; IncludeFiles: boolean);
    procedure BuildFileSystem;

  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
  public
    property IncludeFiles: boolean read FIncludeFiles write FIncludeFiles;
    property OnAccept: TDropFrmAcceptEvent read FOnAccept write FOnAccept;

  end;

var
  frmDrop: TfrmDrop = nil;



implementation
uses
  ShellAPI,
  JvJVCLUtils, // Include/ExcludeTrailingPathDelimiter
  JvJCLUtils; // DirectoryExists, MinimzeFileName

{$R *.dfm}

function GetFullPath(Item: TTreeNode): string;
begin
  Result := '';
  while Item <> nil do
  begin
    Result := Item.Text + '\' + Result;
    Item := Item.Parent;
  end;
  if (Length(Result) < 1) and (Result[2] <> ':') then
    Result := IncludeTrailingPathDelimiter(ExtractFileDrive(Application.Exename)) + Result;
  while (Length(Result) > 3) and (Result[Length(Result)] = '\') do
    SetLength(Result, Length(Result) - 1);
end;

{ TfrmDrop }

procedure TfrmDrop.BuildFileSystem;
var
  S: TStringlist;
  i: integer;
  procedure GetLocalDrives(Strings: TStrings);
  var
    nBufferLength: Cardinal;
    P, lpBuffer: PChar;
  begin
    nBufferLength := GetLogicalDriveStrings(0, nil);
    lpBuffer := AllocMem(nBufferLength);
    try
      GetLogicalDriveStrings(nBufferLength, lpBuffer);
      P := lpBuffer;
      while P^ <> #0 do
      begin
//        if GetDriveType(P) = DRIVE_FIXED then
          Strings.Add(ExcludeTrailingPathDelimiter(P));
        Inc(P, StrLen(P) + 1);
      end;
    finally
      FreeMem(lpBuffer);
    end;
  end;
begin
  tvFolders.Items.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    tvFolders.Items.Clear;
    S := TStringlist.Create;
    try
      GetLocalDrives(S);
      S.Sort;
      for i := 0 to S.Count - 1 do
        BuildFolderList(tvFolders.Items, tvFolders.Items.AddChild(nil, S[i]), S[i], IncludeFiles);
    finally
      S.Free;
    end;
  finally
    tvFolders.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;
//  tvFolders.Items.GetFirstNode.Expand(false);
end;

procedure TfrmDrop.BuildFolderList(Items: TTreeNodes; Parent: TTreeNode; const Root: string; IncludeFiles: boolean);
var
  F,F2: TSearchRec;
  S: string;
  Node:TTreeNode;
begin
  S := IncludeTrailingPathDelimiter(Root);
  if FindFirst(S + '*.*', faDirectory, F) = 0 then
  begin
    repeat
      if (F.Name[1] <> '.') and (F.Attr and faDirectory = faDirectory) then
      begin
        Node := Items.AddChild(Parent, F.Name);
        Node.HasChildren := FindFirst(S + F.Name + '\*.*',faDirectory, F2) = 0;
        if Node.HasChildren then
          FindClose(F2);
      end;
    until FindNext(F) <> 0;
    FindClose(F);
  end;
  if IncludeFiles then
  begin
    if FindFirst(S + '*.*', faAnyFile and not faDirectory, F) = 0 then
    begin
      repeat
        Items.AddChild(Parent, F.Name);
      until FindNext(F) <> 0;
      FindClose(F);
    end;
  end;
end;

procedure TfrmDrop.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if BorderStyle = bsDialog then
    Params.Style := Params.Style and not WS_BORDER;
end;

procedure TfrmDrop.tvFoldersDblClick(Sender: TObject);
begin
  if (tvFolders.Selected <> nil) and (not tvFolders.Selected.HasChildren) then
    btnOK.Click;
end;

procedure TfrmDrop.tvFoldersExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  Node.DeleteChildren;
  Screen.Cursor := crHourGlass;
  tvFolders.Items.BeginUpdate;
  try
    BuildFolderList(tvFolders.Items, Node, GetFullPath(Node), IncludeFiles);
  finally
    Screen.Cursor := crDefault;
    tvFolders.Items.EndUpdate;
  end;
end;

procedure TfrmDrop.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if (Message.Active = WA_INACTIVE) then
    btnCancel.Click;
end;

procedure TfrmDrop.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (ModalResult = mrOK) and Assigned(FOnAccept) then
    FOnAccept(self, -1, GetFullPath(tvFolders.Selected));
//  Action := caFree;
//  frmDrop := nil;
end;

procedure TfrmDrop.btnCancelClick(Sender: TObject);
begin
  if not (fsModal in FormState) then
    Close;
end;

procedure TfrmDrop.btnOKClick(Sender: TObject);
begin
  if not (fsModal in FormState) then
    Close;
end;

procedure TfrmDrop.tvFoldersGetImageIndex(Sender: TObject;
  Node: TTreeNode);
const
  cOpenIcon: array[boolean] of Cardinal = (0, SHGFI_OPENICON);
var
  psfi: TShFileInfo;
begin
  SHGetFileInfo(PChar(GetFullPath(Node)), 0, psfi, sizeof(psfi),
    SHGFI_SMALLICON or SHGFI_SYSICONINDEX or cOpenIcon[Node.Expanded or Node.Selected]);
  Node.ImageIndex := psfi.iIcon;
end;

procedure TfrmDrop.tvFoldersGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
const
  cOpenIcon: array[boolean] of Cardinal = (0, SHGFI_OPENICON);
var
  psfi: TShFileInfo;
begin
  SHGetFileInfo(PChar(GetFullPath(Node)), 0, psfi, sizeof(psfi),
    SHGFI_SMALLICON or SHGFI_SYSICONINDEX or cOpenIcon[Node.Expanded or Node.Selected]);
  Node.SelectedIndex := psfi.iIcon;
end;

procedure TfrmDrop.FormCreate(Sender: TObject);
var
  psfi: TShFileInfo;
begin
  ilSmallIcons.ShareImages := true;
  ilSmallIcons.Handle := SHGetFileInfo('', 0, psfi, sizeof(psfi), SHGFI_SMALLICON or SHGFI_SYSICONINDEX);
  BuildFileSystem;
end;

procedure TfrmDrop.FormShow(Sender: TObject);
begin
  if tvFolders.CanFocus then tvFolders.SetFocus;
end;

procedure TfrmDrop.tvFoldersChange(Sender: TObject; Node: TTreeNode);
begin
  PathLabel.Caption := MinimizeFileName(GetFullPath(Node), Canvas, PathLabel.Width);
end;

end.

