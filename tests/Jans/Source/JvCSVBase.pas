{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCSVBase.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}
unit JvCSVBase;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  stdctrls, extctrls, buttons;

type
  TonCursorChanged = procedure(Sender: Tobject; NameValues: TstringList; Fieldcount: integer) of object;
  TJvCSVBase = class(TComponent)
  private
    { Private declarations }
    DBOpen: boolean;
    DB: Tstringlist;
    DBRecord: TStringlist;
    DBFields: TStringlist;
    DBCursor: integer;
    FonCursorChanged: TonCursorChanged;
    FCSVFileName: string;
    FCSVFieldNames: TStringlist;
    procedure SetonCursorChanged(const Value: TonCursorChanged);
    procedure DoCursorChange;
    procedure SetCSVFileName(const Value: string);
    procedure SetCSVFieldNames(const Value: TStringlist);
    procedure DisPlayFields(NameValues: TStringlist);
  protected
    { Protected declarations }
    procedure CursorChanged(NameValues: TStringList; FieldCount: integer);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DataBaseCreate(aFile: string; FieldNames: TStringList);
    procedure DataBaseOpen(AFile: string);
    procedure DataBaseClose;
    procedure DataBaseRestructure(aFile: string; Fieldnames: TstringList);
    procedure RecordNew;
    procedure RecordGet(var NameValues: TstringList);
    procedure RecordSet(NameValues: TStringList);
    procedure RecordDelete;
    function RecordNext: boolean;
    function RecordPrevious: boolean;
    function RecordFirst: boolean;
    function RecordLast: boolean;
    procedure RecordPost;
    function RecordFind(Atext: string): boolean;
    procedure DisPlay;
  published
    { Published declarations }
    property CSVFileName: string read FCSVFileName write SetCSVFileName;
    property CSVFieldNames: TStringlist read FCSVFieldNames write SetCSVFieldNames;
    property onCursorChanged: TonCursorChanged read FonCursorChanged write SetonCursorChanged;
  end;

  TJvCSVEdit = class(TEdit)
  private
    FCSVDataBase: TJvCSVBase;
    FCSVField: string;
    procedure SetCSVDataBase(const Value: TJvCSVBase);
    procedure SetCSVField(const Value: string);
  protected
    procedure Notification(Acomponent: TComponent; Operation: TOperation); override;
  public
  published
    property CSVDataBase: TJvCSVBase read FCSVDataBase write SetCSVDataBase;
    property CSVField: string read FCSVField write SetCSVField;
  end;

  TJvCSVComboBox = class(TComboBox)
  private
    FCSVField: string;
    FCSVDataBase: TJvCSVBase;
    procedure SetCSVDataBase(const Value: TJvCSVBase);
    procedure SetCSVField(const Value: string);
  protected
    procedure Notification(Acomponent: TComponent; Operation: TOperation); override;
  public
  published
    property CSVDataBase: TJvCSVBase read FCSVDataBase write SetCSVDataBase;
    property CSVField: string read FCSVField write SetCSVField;
  end;

  TJvCSVCheckBox = class(TCheckBox)
  private
    FCSVField: string;
    FCSVDataBase: TJvCSVBase;
    procedure SetCSVDataBase(const Value: TJvCSVBase);
    procedure SetCSVField(const Value: string);
  protected
    procedure Notification(Acomponent: TComponent; Operation: TOperation); override;
  public
  published
    property CSVDataBase: TJvCSVBase read FCSVDataBase write SetCSVDataBase;
    property CSVField: string read FCSVField write SetCSVField;
  end;

  TJvCSVNavigator = class(TCustomControl)
  private
    FbtnFirst: TSpeedButton;
    FbtnPrevious: TspeedButton;
    FbtnFind: TSpeedButton;
    FbtnNext: TSpeedButton;
    FbtnLast: TSpeedbutton;
    FbtnAdd: TSpeedbutton;
    FbtnDelete: TSpeedButton;
    FbtnPost: TSpeedButton;
    FbtnRefresh: TSpeedButton;
    FCSVDataBase: TJvCSVBase;
    procedure CreateButtons;
    procedure btnFirstClick(sender: TObject);
    procedure btnPreviousClick(sender: TObject);
    procedure btnFindClick(sender: TObject);
    procedure btnNextClick(sender: TObject);
    procedure btnLastClick(sender: TObject);
    procedure btnAddClick(sender: TObject);
    procedure btnDeleteClick(sender: TObject);
    procedure btnPostClick(sender: TObject);
    procedure btnRefreshClick(sender: TObject);
    procedure SetCSVDataBase(const Value: TJvCSVBase);

  protected
    procedure Notification(Acomponent: TComponent; Operation: TOperation); override;
  public
    constructor create(AOwner: Tcomponent); override;
    destructor destroy; override;
    procedure CreateWnd; override;
    procedure Resize; override;
  published
    property CSVDataBase: TJvCSVBase read FCSVDataBase write SetCSVDataBase;
  end;

implementation

{$R ..\Resources\JvCSVBase}

{ TJvCSVBase }

constructor TJvCSVBase.Create(AOwner: TComponent);
begin
  inherited;
  DB := tstringlist.create;
  DBRecord := tstringlist.create;
  DBFields := tstringlist.create;
  FCSVFieldNames := TStringList.create;
  DBCursor := -1;
  DBOpen := false;
end;

procedure TJvCSVBase.DataBaseClose;
begin
  FCSVFileName := '';
  DBCursor := -1;
  DoCursorChange;
end;

procedure TJvCSVBase.DataBaseCreate(aFile: string; FieldNames: TStringList);
var
  newfile: string;
  Alist: tstringlist;
begin
  newfile := changefileext(aFile, '.csv');
  if fileexists(newfile) then
    if messagedlg('Replace existing database?', mtconfirmation, [mbyes, mbno], 0) = mrno then exit;
  alist := tstringlist.create;
  if (FieldNames <> nil) then
    if FieldNames.count > 0 then
      alist.Text := FieldNames.CommaText;
  alist.SaveToFile(newfile);
end;

procedure TJvCSVBase.DataBaseOpen(AFile: string);
begin
  if not fileexists(Afile) then
    DataBaseCreate(Afile, nil);
  FCSVFileName := AFile;
  DB.LoadFromFile(CSVFileName);
  DBCursor := -1;
  DBFields.Clear;
  DBRecord.Clear;
  if DB.Count > 0 then
  begin
    DBCursor := 0;
    DBFields.CommaText := DB[0];
    FCSVFieldNames.commatext := DB[0];
    if DB.count > 1 then
    begin
      DBCursor := 1;
      DBRecord.commatext := DB[DBCursor];
      DoCursorChange;
    end;
  end;
end;

procedure TJvCSVBase.DataBaseRestructure(aFile: string; Fieldnames: TstringList);
var
  OldBase: tstringlist;
  OldRec: Tstringlist;
  OldFields: TStringlist;
  NewBase: tstringlist;
  NewRec: TStringlist;
  NewFields: TStringlist;
  index, rec, fld: integer;
begin
  DataBaseClose;
  if Fieldnames.count = 0 then
  begin
    showmessage('no fields defined');
    exit;
  end;
  OldBase := tstringlist.create;
  OldRec := Tstringlist.create;
  OldFields := TStringlist.create;
  NewBase := tstringlist.create;
  NewRec := TStringlist.create;
  NewFields := TStringlist.create;
  OldBase.LoadFromFile(afile);
  if OldBase.count = 0 then
  begin
    NewFields.assign(FieldNames);
    NewBase.Append(NewFields.commatext);
  end
  else
  begin
    //restructure
    OldFields.CommaText := Oldbase[0];
    NewFields.assign(FieldNames);
    NewBase.Append(NewFields.commatext);
    if OldBase.count > 1 then
      for rec := 1 to OldBase.count - 1 do
      begin
        OldRec.CommaText := OldBase[rec];
        Newrec.Clear;
        for fld := 0 to NewFields.count - 1 do
        begin
          index := OldFields.IndexOf(NewFields[fld]);
          if index = -1 then
            NewRec.Append('-')
          else
            NewRec.Append(OldRec[index]);
        end;
        NewBase.Append(NewRec.commatext);
      end;
  end;
  NewBase.SaveToFile(afile);
  OldBase.free;
  OldRec.free;
  OldFields.free;
  NewBase.free;
  NewRec.free;
  NewFields.free;
end;

destructor TJvCSVBase.Destroy;
begin
  DB.Free;
  DBRecord.free;
  DBFields.Free;
  FCSVFieldNames.Free;
  inherited;
end;

procedure TJvCSVBase.RecordNew;
var
  i: integer;
begin
  if DBCursor <> -1 then
  begin
    DBRecord.Clear;
    for i := 0 to DBFields.Count - 1 do
      DBRecord.Append('-');
    DB.Append(DBRecord.commatext);
    DBCursor := DB.count - 1;
    DB.SaveToFile(CSVFileName);
    DoCursorChange;
  end;
end;

procedure TJvCSVBase.RecordDelete;
begin
  if DBCursor > 0 then
  begin
    DB.Delete(DBCursor);
    if DBCursor > (DB.count - 1) then dec(DBCursor);
    if DBCursor > 0 then
    begin
      DBRecord.commatext := DB[DBCursor];
      DB.SaveToFile(CSVFileName);
    end;
    DoCursorChange;
  end;
end;

function TJvCSVBase.RecordFind(Atext: string): boolean;
var
  i, from: integer;
  fs: string;
begin
  result := false;
  if DBCursor < 1 then exit;
  if DBCursor < (DB.Count - 1) then
  begin
    from := DBCursor + 1;
    fs := lowercase(AText);
    for i := from to DB.Count - 1 do
      if pos(fs, lowercase(DB[i])) > 0 then
      begin
        DBCursor := i;
        DBRecord.commatext := DB[DBCursor];
        result := true;
        DoCursorChange;
        break;
      end;
  end;
end;

function TJvCSVBase.RecordFirst: boolean;
begin
  result := false;
  if DBCursor <> -1 then
    if DB.count > 1 then
    begin
      DBCursor := 1;
      DBRecord.commatext := DB[DBCursor];
      result := true;
      DoCursorChange;
    end;
end;

procedure TJvCSVBase.RecordGet(var NameValues: TstringList);
var
  i: integer;
begin
  NameValues.clear;
  if DBCursor < 1 then exit;
  for i := 0 to DBFields.Count - 1 do
    NameValues.Append(DBFields[i] + '=' + DBRecord[i]);
end;

function TJvCSVBase.RecordLast: boolean;
begin
  result := false;
  if DBCursor <> -1 then
    if DB.count > 1 then
    begin
      DBCursor := DB.count - 1;
      DBRecord.commatext := DB[DBCursor];
      result := true;
      DoCursorChange;
    end;
end;

function TJvCSVBase.RecordNext: boolean;
begin
  result := false;
  if DBCursor <> -1 then
  begin
    if DBCursor < (DB.Count - 1) then
    begin
      inc(DBCursor);
      DBRecord.commatext := DB[DBCursor];
      result := true;
      DoCursorChange;
    end;
  end;

end;

function TJvCSVBase.RecordPrevious: boolean;
begin
  result := false;
  if DBCursor <> -1 then
  begin
    if DBCursor > 1 then
    begin
      dec(DBCursor);
      DBRecord.commatext := DB[DBCursor];
      result := true;
      DoCursorChange;
    end;
  end;

end;

procedure TJvCSVBase.RecordSet(NameValues: TStringList);
var
  i, index: integer;
  FieldName: string;
begin
  if NameValues.count > 0 then
  begin
    for i := 0 to NameValues.count - 1 do
    begin
      FieldName := NameValues.names[i];
      index := DBFields.IndexOf(FieldName);
      if index <> -1 then
        DBRecord[index] := NameValues.Values[FieldName];
    end;
    DB[DBCursor] := DBRecord.CommaText;
    DB.SaveToFile(CSVFileName);
  end;
end;

procedure TJvCSVBase.SetonCursorChanged(const Value: TonCursorChanged);
begin
  FonCursorChanged := Value;
end;

procedure TJvCSVBase.CursorChanged(NameValues: TStringList; FieldCount: integer);
begin
  if assigned(onCursorchanged) then
    onCursorChanged(self, NameValues, FieldCount);
end;

procedure TJvCSVBase.DoCursorChange;
var
  NameValues: TStringlist;
begin
  NameValues := TStringList.create;
  try
    RecordGet(NameValues);
    DisPlayFields(NameValues);
    CursorChanged(NameValues, NameValues.count);
  finally
    NameValues.Free;
  end;
end;

procedure TJvCSVBase.DisPlayFields(NameValues: TStringlist);
var
  Aform: TForm;
  i, index: integer;
  ed: TJvCSVEdit;
  cbo: TJvCSVComboBox;
  ck: TJvCSVCheckBox;
  AField: string;
begin
  Aform := TForm(self.Owner);
  for i := 0 to aForm.ComponentCount - 1 do
    if aform.Components[i].classname = 'TJvCSVEdit' then
    begin
      ed := TJvCSVEdit(aform.Components[i]);
      if ed.CSVDataBase = self then
      begin
        Afield := ed.CSVField;
        index := CSVFieldNames.IndexOf(AField);
        if index <> -1 then
          if DBCursor > 0 then
            ed.Text := DBRecord[index]
          else
            ed.Text := '[' + Afield + ']';
      end;
    end
    else if aform.Components[i].classname = 'TJvCSVComboBox' then
    begin
      cbo := TJvCSVComboBox(aform.Components[i]);
      if cbo.CSVDataBase = self then
      begin
        Afield := cbo.CSVField;
        index := CSVFieldNames.IndexOf(AField);
        if index <> -1 then
          if DBCursor > 0 then
            cbo.Text := DBRecord[index]
          else
            cbo.Text := '[' + Afield + ']';
      end;
    end
    else if aform.Components[i].classname = 'TJvCSVCheckBox' then
    begin
      ck := TJvCSVCheckBox(aform.Components[i]);
      if ck.CSVDataBase = self then
      begin
        Afield := ck.CSVField;
        index := CSVFieldNames.IndexOf(AField);
        if index <> -1 then
          if DBCursor > 0 then
            ck.checked := DBRecord[index] = 'true'
          else
            ck.Checked := false;
      end;
    end;
end;

procedure TJvCSVBase.SetCSVFileName(const Value: string);
begin
  if value <> FCSVFileName then
  begin
    DatabaseClose;
    FCSVFileName := Value;
    if fileexists(CSVFileName) then
      DataBaseOpen(CSVFileName)
    else
      DataBaseCreate(CSVFileName, nil);
  end;
end;

procedure TJvCSVBase.DisPlay;
begin
  doCursorChange;
end;

procedure TJvCSVBase.RecordPost;
var
  Aform: TForm;
  i, index: integer;
  ed: TJvCSVEdit;
  cbo: TJvCSVComboBox;
  ck: TJvCSVCheckBox;
  AField: string;
begin
  if DBCursor < 1 then exit;
  Aform := TForm(self.Owner);
  for i := 0 to aForm.ComponentCount - 1 do
    if aform.Components[i].classname = 'TJvCSVEdit' then
    begin
      ed := TJvCSVEdit(aform.Components[i]);
      if ed.CSVDataBase = self then
      begin
        Afield := ed.CSVField;
        index := CSVFieldNames.IndexOf(AField);
        if index <> -1 then
          DBRecord[index] := ed.text;
      end;
    end
    else if aform.Components[i].classname = 'TJvCSVComboBox' then
    begin
      cbo := TJvCSVComboBox(aform.Components[i]);
      if cbo.CSVDataBase = self then
      begin
        Afield := cbo.CSVField;
        index := CSVFieldNames.IndexOf(AField);
        if index <> -1 then
          DBRecord[index] := cbo.text;
      end;
    end
    else if aform.Components[i].classname = 'TJvCSVCheckBox' then
    begin
      ck := TJvCSVCheckBox(aform.Components[i]);
      if ck.CSVDataBase = self then
      begin
        Afield := ck.CSVField;
        index := CSVFieldNames.IndexOf(AField);
        if index <> -1 then
          if ck.Checked then
            DBRecord[index] := 'true'
          else
            DBRecord[index] := 'false';
      end;
    end;

  DB[DBCursor] := DBRecord.CommaText;
  DB.SaveToFile(CSVFileName);
end;

{ TCSVFileNameProperty }

procedure TJvCSVBase.SetCSVFieldNames(const Value: TStringlist);
var
  oldfile: string;
begin
  if (CSVFileName <> '') and (value.Count > 0) then
  begin
    OldFile := CSVFileName;
    DataBaseClose;
    FCSVFieldNames.Assign(Value);
    DataBaseRestructure(oldFile, value);
    DataBaseOpen(OldFile);
  end;
end;

{ TJvCSVEdit }

procedure TJvCSVEdit.Notification(Acomponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FCSVDataBase) then
  begin
    FCSVDataBase := nil;
    FCSVField := '';
  end;
end;

procedure TJvCSVEdit.SetCSVDataBase(const Value: TJvCSVBase);
begin
  FCSVDataBase := Value;
end;

procedure TJvCSVEdit.SetCSVField(const Value: string);
begin
  if value <> FCSVField then
  begin
    FCSVField := Value;
    if assigned(FCSVDataBase) then
      CSVDataBase.display;
  end;
end;

{ TCSVFieldProperty }

{ TJvCSVNavigator }

procedure TJvCSVNavigator.btnAddClick(sender: TObject);
begin
  if assigned(FCSVDataBase) then
    CSVDataBase.RecordNew;

end;

procedure TJvCSVNavigator.btnDeleteClick(sender: TObject);
begin
  if assigned(FCSVDataBase) then
    CSVDataBase.RecordDelete;

end;

procedure TJvCSVNavigator.btnFindClick(sender: TObject);
var
  Atext: string;
begin
  if assigned(FCSVDataBase) then
  begin
    Atext := inputbox('CSV DataBase', 'Find Text:', '');
    if Atext <> '' then
      CSVDataBase.RecordFind(Atext);
  end;

end;

procedure TJvCSVNavigator.btnFirstClick(sender: TObject);
begin
  if assigned(FCSVDataBase) then
    CSVDataBase.RecordFirst;
end;

procedure TJvCSVNavigator.btnLastClick(sender: TObject);
begin
  if assigned(FCSVDataBase) then
    CSVDataBase.RecordLast;

end;

procedure TJvCSVNavigator.btnNextClick(sender: TObject);
begin
  if assigned(FCSVDataBase) then
    CSVDataBase.RecordNext;

end;

procedure TJvCSVNavigator.btnPostClick(sender: TObject);
begin
  if assigned(FCSVDataBase) then
    CSVDataBase.RecordPost;
end;

procedure TJvCSVNavigator.btnPreviousClick(sender: TObject);
begin
  if assigned(FCSVDataBase) then
    CSVDataBase.RecordPrevious;

end;

procedure TJvCSVNavigator.btnRefreshClick(sender: TObject);
begin
  if assigned(FCSVDataBase) then
    CSVDataBase.display;

end;

constructor TJvCSVNavigator.create(AOwner: Tcomponent);
begin
  inherited;
  height := 24;
  width := 217;
  CreateButtons;
  Caption := '';
end;

procedure TJvCSVNavigator.CreateButtons;

procedure ib(b: TSpeedButton);
  begin
    b.Width := 23;
    b.height := 22;
    b.flat := true;
    b.parent := self;
    b.top := 1;
    showhint := true;
  end;

begin

  FbtnFirst := TSpeedButton.Create(Self);
  with FbtnFirst do
  begin
    ib(FbtnFirst);
    Glyph.LoadFromResourceName(HInstance, 'FIRST');
    Left := 1;
    OnClick := BtnFirstClick;
    hint := 'First';
  end;

  FbtnPrevious := TSpeedButton.Create(Self);
  with FbtnPrevious do
  begin
    ib(FbtnPrevious);
    Glyph.LoadFromResourceName(HInstance, 'PREVIOUS');
    Left := 25;
    OnClick := BtnPreviousClick;
    hint := 'Previous';
  end;

  FbtnFind := TSpeedButton.Create(Self);
  with FbtnFind do
  begin
    ib(FbtnFind);
    Glyph.LoadFromResourceName(HInstance, 'FIND');
    Left := 49;
    OnClick := BtnFindClick;
    hint := 'Find';
  end;

  FbtnNext := TSpeedButton.Create(Self);
  with FbtnNext do
  begin
    ib(FbtnNext);
    Glyph.LoadFromResourceName(HInstance, 'NEXT');
    Left := 73;
    OnClick := BtnNextClick;
    hint := 'Next';
  end;

  FbtnLast := TSpeedButton.Create(Self);
  with FbtnLast do
  begin
    ib(FbtnLast);
    Glyph.LoadFromResourceName(HInstance, 'LAST');
    Left := 97;
    OnClick := BtnLastClick;
    hint := 'Last';
  end;

  FbtnAdd := TSpeedButton.Create(Self);
  with FbtnAdd do
  begin
    ib(FbtnAdd);
    Glyph.LoadFromResourceName(HInstance, 'ADD');
    Left := 121;
    OnClick := BtnAddClick;
    hint := 'Add';
  end;

  FbtnDelete := TSpeedButton.Create(Self);
  with FbtnDelete do
  begin
    ib(FbtnDelete);
    Glyph.LoadFromResourceName(HInstance, 'DELETE');
    Left := 145;
    OnClick := BtnDeleteClick;
    hint := 'Delete';
  end;

  FbtnPost := TSpeedButton.Create(Self);
  with FbtnPost do
  begin
    ib(FbtnPost);
    Glyph.LoadFromResourceName(HInstance, 'POST');
    Left := 169;
    OnClick := BtnPostClick;
    hint := 'Post';
  end;

  FbtnRefresh := TSpeedButton.Create(Self);
  with FbtnRefresh do
  begin
    ib(FbtnRefresh);
    Glyph.LoadFromResourceName(HInstance, 'REFRESH');
    Left := 193;
    OnClick := BtnRefreshClick;
    hint := 'Refresh';
  end;

end;

procedure TJvCSVNavigator.CreateWnd;
begin
  inherited;
  caption := '';
end;

destructor TJvCSVNavigator.destroy;
begin
  inherited;

end;

procedure TJvCSVNavigator.Notification(Acomponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FCSVDataBase) then
  begin
    FCSVDataBase := nil;
  end;

end;

procedure TJvCSVNavigator.Resize;
begin
  inherited;
  height := 24;
  if width < 221 then width := 221;
  if assigned(onresize) then onresize(self);
end;

procedure TJvCSVNavigator.SetCSVDataBase(const Value: TJvCSVBase);
begin
  FCSVDataBase := Value;
end;

{ TJvCSVComboBox }

procedure TJvCSVComboBox.Notification(Acomponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FCSVDataBase) then
  begin
    FCSVDataBase := nil;
    FCSVField := '';
  end;
end;

procedure TJvCSVComboBox.SetCSVDataBase(const Value: TJvCSVBase);
begin
  FCSVDataBase := Value;
end;

procedure TJvCSVComboBox.SetCSVField(const Value: string);
begin
  if value <> FCSVField then
  begin
    FCSVField := Value;
    if assigned(FCSVDataBase) then
      CSVDataBase.display;
  end;
end;

{ TJvCSVCheckBox }

procedure TJvCSVCheckBox.Notification(Acomponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FCSVDataBase) then
  begin
    FCSVDataBase := nil;
    FCSVField := '';
  end;
end;

procedure TJvCSVCheckBox.SetCSVDataBase(const Value: TJvCSVBase);
begin
  FCSVDataBase := Value;
end;

procedure TJvCSVCheckBox.SetCSVField(const Value: string);
begin
  if value <> FCSVField then
  begin
    FCSVField := Value;
    if assigned(FCSVDataBase) then
      CSVDataBase.display;
  end;
end;

end.
