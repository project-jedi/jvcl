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
{$I jvcl.inc}
unit main;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF COMPILER6_UP}Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, JvUIB, StdCtrls, JvUIBSQLParser, ComCtrls, JvComponent;

type
  TMainForm = class(TForm)
    Script: TJvUIBScript;
    DataBase: TJvUIBDataBase;
    JvUIBTransaction1: TJvUIBTransaction;
    Button1: TButton;
    ProgressBar: TProgressBar;
    Edit: TEdit;
    StatusBar: TStatusBar;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure ScriptParse(Sender: TObject; NodeType: TSQLNodeType;
      const Statement: String; Postion, Count: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  NodeInfos: array[TSQLNodeType] of string =
   ('List',
    'Select',
    'Alter Exception',
    'Alter Table',
    'Alter Trigger',
    'Alter Procedure',
    'Alter Database',
    'Alter Domain',
    'Alter Index',
    'Read Blob',
    'Insert Blob',
    'Commit',
    'Rollback',
    'Create Exception',
    'Create Index',
    'Create Procedure',
    'Create Table',
    'Create Trigger',
    'Create View',
    'Create Generator',
    'Create Database',
    'Create Domain',
    'Create Shadow',
    'Create Role',
    'Declare Filter',
    'Declare Function',
    'Delete Searched',
    'Delete Positioned',
    'Drop Exception',
    'Drop Index',
    'Drop Procedure',
    'Drop Table',
    'Drop Trigger',
    'Drop View',
    'Drop Filter',
    'Drop Domain',
    'Drop External',
    'Drop Shadow',
    'Drop Role',
    'Drop Generator',
    'Grant',
    'Revoke',
    'Insert',
    'Invoke Procedure',
    'Recreate Procedure',
    'Recreate Table',
    'Recreate View',
    'Replace',
    'Savepoint Set',
    'Savepoint Release',
    'Savepoint Undo',
    'Set Transaction',
    'Set Generator',
    'Set Statistics',
    'Update Searched',
    'Update Positioned',
    'Debug',

    'Set Names',
    'Set SqlDialect',
    'Set AutoDDL',
    'Connect',

    'Name',
    'Username',
    'PassWord',
    'PageSize',
    'Length'

   );

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
var c: cardinal;
begin
  if FileExists('d:\database.db') then
    DeleteFile('d:\database.db');
  c := GetTickCount;
  Script.ExecuteScript;
  Edit.text := inttostr(GetTickCount - c) + ' ms';
  DataBase.Connected := False;
end;

procedure TMainForm.ScriptParse(Sender: TObject; NodeType: TSQLNodeType;
  const Statement: String; Postion, Count: Integer);
begin
  ProgressBar.Max := Count;
  ProgressBar.Position := Postion;
  StatusBar.SimpleText := NodeInfos[NodeType];
end;

end.
