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

{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1996 AO ROSNO                       }
{     Copyright (c) 1997 Master-Bank                    }
{                                                       }
{*******************************************************}
unit OpenDlg;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, Mask, JvToolEdit, DB, ExtCtrls, JvBDELists, 
  DBTables, JvComponent, JvFormPlacement, JvDBLookup, JvPicClip, JvExMask,
  JvExControls;

type
  TOpenDatabaseDlg = class(TForm)
    Bevel1: TBevel;
    DataSource1: TDataSource;
    rxDBLookupCombo1: TJvDBLookupCombo ;
    DirectoryEdit1: TJvDirectoryEdit ;
    Label1: TLabel;
    Label2: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    FormStorage1: TJvFormStorage ;
    JvPicclip: TJvPicClip ;
    procedure rxDBLookupCombo1Change(Sender: TObject);
    procedure DirectoryEdit1Change(Sender: TObject);
    procedure DBLookupComboGetImage(Sender: TObject; IsEmpty: Boolean;
      var Graphic: TGraphic; var TextMargin: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    LangDrivList:TJvLangDrivList; //!!!
    DatabaseList:TJvBDEItems; //!!!
    function GetDatabaseName: string;
  public
    { Public declarations }
    property DatabaseName: string read GetDatabaseName;
  end;

function GetOpenDatabase(var DBName: string): Boolean;

implementation

{$R *.DFM}

uses SysUtils, JvDBUtils, JvBdeUtils;

function GetOpenDatabase(var DBName: string): Boolean;
begin
  Result := False;
  with TOpenDatabaseDlg.Create(Application) do
  try
    if ShowModal = mrOk then begin
      DBName := DatabaseName;
      Result := DBName <> '';
    end;
  finally
    Free;
  end;
end;

{ TOpenDatabaseDlg }

function TOpenDatabaseDlg.GetDatabaseName: string;
begin
  Result := rxDBLookupCombo1.DisplayValue;
  if Result = '' then Result := DirectoryEdit1.Text;
end;

procedure TOpenDatabaseDlg.rxDBLookupCombo1Change(Sender: TObject);
begin
  if DataSetFindValue(DatabaseList, rxDBLookupCombo1.Value, 'NAME') then
    DirectoryEdit1.Text := DatabaseList.FieldByName('PHYNAME').AsString;
end;

procedure TOpenDatabaseDlg.DirectoryEdit1Change(Sender: TObject);
begin
  if DirectoryEdit1.Text <> '' then begin
    if DataSetFindValue(DatabaseList, DirectoryEdit1.Text, 'PHYNAME') then
      rxDBLookupCombo1.Value := DatabaseList.FieldByName('NAME').AsString
    else rxDBLookupCombo1.ResetField;
  end;
end;

procedure TOpenDatabaseDlg.DBLookupComboGetImage(Sender: TObject;
  IsEmpty: Boolean; var Graphic: TGraphic; var TextMargin: Integer);
begin
  TextMargin := JvPicclip.Width + 2;
  if not IsEmpty then begin
    if CompareText(DatabaseList.FieldByName('DBTYPE').AsString, 'STANDARD') = 0
      then Graphic := JvPicclip.GraphicCell[1]
    else Graphic := JvPicclip.GraphicCell[0];
  end;
end;

procedure TOpenDatabaseDlg.FormCreate(Sender: TObject);
begin
{$IFDEF WIN32}
  DirectoryEdit1.DialogText := 'Select a path to the target database.';
  DirectoryEdit1.DialogKind := dkWin32;
{$ENDIF}
  DatabaseList := TJvBDEItems.Create(self);
  DatabaseList.ItemType := bdDatabases;
  DatabaseList.Open;
  LangDrivList := TJvLangDrivList.Create(self);
  LangDrivList.Open;
  DataSource1.DataSet := DatabaseList;
end;

end.
