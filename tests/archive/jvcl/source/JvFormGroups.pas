{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormGroups.PAS, released on 2001-02-28.

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

unit JvFormGroups;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, FileCtrl, Buttons,
  JvDirectories, JvSpeedButton, JvComponent;

type
  TFormGroup = class(TForm)
    Image1: TImage;
    Bevel1: TBevel;
    StaticText1: TStaticText;
    Edit1: TEdit;
    TreeView1: TTreeView;
    BUDirectories1: TJvDirectories;
    BUButton1: TJvSpeedButton;
    BUButton2: TJvSpeedButton;
    BUButton3: TJvSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    procedure Search(Path: string; Node: TTreeNode);
  public
    GroupName: string;
  end;

implementation

{$R *.DFM}

{************************************************************}

procedure TFormGroup.Search(Path: string; Node: TTreeNode);
var
  t: TSearchRec;
  res: Integer;
  n: TTreeNode;
  ts: TStringList;
begin
  ts := TStringList.Create;
  if (Path <> '') and (Path[length(Path)] <> '\') then
    Path := Path + '\';
  res := FindFirst(Path + '*.*', faAnyFile, t);
  while res = 0 do
  begin
    if (t.Name <> '.') and (t.Name <> '..') then
      if DirectoryExists(Path + t.Name) then //include FileCtrl for this function
        ts.Add(t.Name);
    res := FindNext(t);
  end;
  FindClose(t);

  ts.Sort;
  for res := 0 to ts.Count - 1 do
  begin
    n := TreeView1.Items.AddChild(Node, ts[res]);
    n.Text := ts[res];
    Search(Path + ts[res], n);
  end;

  ts.Free;
end;

{************************************************************}

procedure TFormGroup.FormShow(Sender: TObject);
begin
  TreeView1.Items.Clear;
  Search(BUDirectories1.Programs, nil);
end;

{************************************************************}

procedure TFormGroup.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  Edit1.Text := GroupName;
  while Node <> nil do
  begin
    Edit1.Text := Node.Text + '\' + Edit1.Text;
    Node := Node.Parent;
  end;
end;

{************************************************************}

procedure TFormGroup.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := Tag = 1;
end;

end.
