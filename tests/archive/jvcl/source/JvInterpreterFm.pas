{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreterFm.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : JVCL Interpreter version 2
Component   : form runner for JvInterpreter

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

{ history (JVCL Library versions):
  1.10:
   - first release;
  1.12:
   - more smart interface-part reducementer -
     method MakeCompatibleUnit;
  1.31.3 (JVCL Library 1.31 with update 3):
   - support for Delphi5 text DFM files.
  1.52:
   - fixed memory bug;
  1.52.4:
   - previous memory bug fix was moved to JvInterpreter.pas unit;
  1.60:
   - forms, placed in used units, are supported;
   - method MakeCompatibleUnit has been removed;
  1.61:
   - fixed bug: local variables in methods overrieded by form memebers;
     this bug prevented MDI forms from "Action := caFree" code to work
     (thanks to Ivan Ravin);
  2.00:
    - loading of inherited forms added by Cerny Robert; 

}


unit JvInterpreterFm;

interface

uses Windows, SysUtils, Classes, Controls, Forms,
  JvInterpreter, JvInterpreterParser;

type

  TJvInterpreterGetDfmFileName = procedure(Sender: TObject; UnitName: string;
    var FileName: string; var Done: Boolean) of object;
  TJvInterpreterCreateDfmStream = procedure(Sender: TObject; UnitName: string;
    var Stream: TStream; var Done: Boolean) of object;
  TJvInterpreterFreeDfmStream = procedure(Sender: TObject; Stream: TStream) of object;

  TJvInterpreterFm = class;

  TJvInterpreterForm = class(TForm)
  private
    FJvInterpreterFm: TJvInterpreterFm;
    FMethodList: TList;
    FreeFJvInterpreterFm: Boolean;
    FClassIdentifer: string;
    FUnitName: string;
    procedure FixupMethods;
  protected
    procedure ReadState(Reader: TReader); override;
  public
    {$IFDEF COMPILER4_UP}
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    {$ELSE}
    constructor CreateNew(AOwner: TComponent; Dummy: Integer);
    {$ENDIF COMPILER4_UP}
    destructor Destroy; override;
    property JvInterpreterFm: TJvInterpreterFm read FJvInterpreterFm write FJvInterpreterFm;
  end;

  TJvInterpreterFm = class(TJvInterpreterProgram)
  private
    FForm: TJvInterpreterForm;
    FFileName: string;
    FInterfaceUses: Boolean;
    FOnGetDfmFileName: TJvInterpreterGetDfmFileName;
    FOnCreateDfmStream: TJvInterpreterCreateDfmStream;
    FOnFreeDfmStream: TJvInterpreterFreeDfmStream;
    procedure LoadForm(AForm: TJvInterpreterForm);
  protected
    function GetValue(Identifer: string; var Value: Variant; var Args: TArgs)
      : Boolean; override;
    function SetValue(Identifer: string; const Value: Variant; var Args: TArgs)
      : Boolean; override;
    function GetUnitSource(UnitName: string; var Source: string): boolean;
      override;
    procedure CreateDfmStream(const UnitName: string; var Stream: TStream); dynamic;
    procedure FreeDfmStream(Stream: TStream); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run; override;
    function MakeForm(const FileName: TFileName): TForm;
    function MakeInheritedForm(F: TJvInterpreterForm; const FileName: TFileName): TForm;
    function RunForm(const FileName: TFileName): TForm;
    function RunFormModal(const FileName: TFileName): TModalResult;
    function RunUnit(const FileName: TFileName): Variant;
    procedure RunReportPreview(const FileName: string);
    property Form: TJvInterpreterForm read FForm;
  published
    property OnGetDfmFileName: TJvInterpreterGetDfmFileName read FOnGetDfmFileName write FOnGetDfmFileName;
    property OnCreateDfmStream: TJvInterpreterCreateDfmStream read FOnCreateDfmStream write FOnCreateDfmStream;
    property OnFreeDfmStream: TJvInterpreterFreeDfmStream read FOnFreeDfmStream write FOnFreeDfmStream;
    property InterfaceUses: Boolean read FInterfaceUses write FInterfaceUses default False;
  end;

  function JvInterpreterRunFormModal(const FileName: TFileName): TModalResult;
  function JvInterpreterRunForm(const FileName: TFileName): TForm;
  function JvInterpreterMakeForm(const FileName: TFileName): TForm;
  function JvInterpreterRunUnit(const FileName: TFileName): Variant;
  procedure JvInterpreterRunReportPreview(const FileName: string);
  procedure JvInterpreterRunReportPreview2(const FileName: string; JvInterpreterProgram: TJvInterpreterFm);

  procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);


const
  ieImplementationNotFound = 401;


var
  JvInterpreterRunReportPreviewProc: procedure (const FileName: string);
  JvInterpreterRunReportPreview2Proc: procedure (const FileName: string; JvInterpreterProgram: TJvInterpreterFm);

implementation

uses Consts, TypInfo, JvInterpreterConst;


function LoadTextFile(const FileName : TFileName): string;
begin
  with TStringList.Create do
  try
    LoadFromFile(FileName);
    Result := Text;
  finally
    Free;
  end;
end;

function AddSlash2(const Dir : TFileName) : string;
begin
  Result := Dir;
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> '\') then
    Result := Dir +'\';
end;

function FindInPath(const FileName, PathList: string): TFileName;
var
  i: Integer;
  S: string;
begin
  i := 0;
  S := SubStr(PathList, i, ';');
  while S <> '' do                  
  begin
    Result := AddSlash2(S) + FileName;
    if FileExists(Result) then
      Exit;
    inc(i);
    S := SubStr(PathList, i, ';');
  end;
  Result := '';
end;


type

  TJvInterpreterReader = class(TReader)
  protected
    function FindMethod(Root: TComponent; const MethodName: string): Pointer;
      override;
  end;

  THackAdapter = class(TJvInterpreterAdapter);
  

function TJvInterpreterReader.FindMethod(Root: TComponent; const MethodName: string)
  : Pointer;
begin
  Result := NewStr(MethodName);
  TJvInterpreterForm(Root).FMethodList.Add(Result);
end;

{$IFDEF COMPILER4_UP}
constructor TJvInterpreterForm.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
{$ELSE}
constructor TJvInterpreterForm.CreateNew(AOwner: TComponent; Dummy: Integer);
{$ENDIF COMPILER4_UP}
begin
  FMethodList := TList.Create;
 {$IFDEF Delphi}
  inherited CreateNew(AOwner);
 {$ELSE}
  inherited CreateNew(AOwner, Dummy);
 {$ENDIF}
end;    { Create }

destructor TJvInterpreterForm.Destroy;
var
  i: Integer;
begin
  for i := 0 to FMethodList.Count - 1 do    { Iterate }
    DisposeStr(FMethodList[i]);
  FMethodList.Free;
  inherited Destroy;
  if FreeFJvInterpreterFm then
    FJvInterpreterFm.Free;
end;    { Destroy }

procedure TJvInterpreterForm.FixupMethods;

  procedure ReadProps(Com: TComponent);
  var
    TypeInf  : PTypeInfo;
    TypeData : PTypeData;
    PropList : PPropList;
    NumProps : word;
    i: Integer;
    F: Integer;
    Method: TMethod;
  begin
    TypeInf := Com.ClassInfo;
    TypeData := GetTypeData(TypeInf);
    NumProps := TypeData^.PropCount;
    GetMem(PropList, NumProps*sizeof(pointer));
    try
      GetPropInfos(TypeInf, PropList);
      for i := 0 to NumProps-1 do
        if PropList^[i].PropType^.Kind = tkMethod then
        begin
          Method := GetMethodProp(Com, PropList^[i]);
          if Method.Data = Self then
          begin
            F := FMethodList.IndexOf(Method.Code);
            if F > -1 then
            begin
              SetMethodProp(Com, PropList^[i], TMethod(FJvInterpreterFm.NewEvent(
                FUnitName,
                PString(FMethodList[F])^, PropList^[i]^.PropType^.Name,
                Self)));
            end;
          end;
        end;
    finally
      FreeMem(PropList, NumProps*sizeof(pointer));
    end;
  end;

var
  i: Integer;
begin
  if FJvInterpreterFm = nil then Exit; {+RWare}
  ReadProps(Self);
  for i := 0 to ComponentCount - 1 do    { Iterate }
    ReadProps(Components[i]);
end;

procedure TJvInterpreterForm.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  FixupMethods;
end;

function JvInterpreterReadComponentRes(var Stream: TStream; Instance: TComponent)
  : TComponent;
var
  JvInterpreterReader: TJvInterpreterReader;
 {$IFDEF COMPILER5_UP}
  TmpStream: TMemoryStream;
 {$ENDIF COMPILER5_UP}
begin
 {$IFDEF COMPILER5_UP}
  if TestStreamFormat(Stream) = sofText then
  begin
    TmpStream := TMemoryStream.Create;
    ObjectTextToResource(Stream, TmpStream);
    Stream.Free;
    Stream := TmpStream;
    Stream.Position := 0;
  end;
 {$ENDIF COMPILER5_UP}

  Stream.ReadResHeader;
  JvInterpreterReader := TJvInterpreterReader.Create(Stream, 4096);
  try
    Result := JvInterpreterReader.ReadRootComponent(Instance);
  finally
    JvInterpreterReader.Free;
  end;
end;


{ ********************* TJvInterpreterFm **********************}
constructor TJvInterpreterFm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;    { Create }

destructor TJvInterpreterFm.Destroy;
begin
  inherited Destroy;
end;    { Destroy }

function TJvInterpreterFm.MakeForm(const FileName: TFileName): TForm;
var
  S: string;
  UnitName: string;
begin
  FFileName := FileName;
  UnitName := ChangeFileExt(ExtractFileName(FFileName), '');
  if not (GetUnitSource(FFileName, S) or GetUnitSource(UnitName, S)) then
    JvInterpreterErrorN(ieUnitNotFound, -1, UnitName);
  Source := S;
  Compile;
  {$IFDEF COMPILER4_UP}
  FForm := TJvInterpreterForm.CreateNew(Application);
  {$ELSE}
  FForm := TJvInterpreterForm.CreateNew(Application, 0);
  {$ENDIF COMPILER4_UP}
  FForm.FUnitName := UnitName;
  LoadForm(FForm);
  Result := FForm;
end;    { MakeForm }

function TJvInterpreterFm.MakeInheritedForm(F: TJvInterpreterForm; const FileName: TFileName): TForm;
var
  S: string;
  UnitName: string;
begin
  FFileName := FileName;
  UnitName := ChangeFileExt(ExtractFileName(FFileName), '');
  if not (GetUnitSource(FFileName, S) or GetUnitSource(UnitName, S)) then
    JvInterpreterErrorN(ieUnitNotFound, -1, UnitName);
  Source := S;
  Compile;
  FForm := F;
  FForm.FUnitName := UnitName;
  LoadForm(FForm);
  Result := FForm;
end;    { MakeInheritedForm }

procedure TJvInterpreterFm.CreateDfmStream(const UnitName: string; var Stream: TStream);
var
  Done: Boolean;
  DfmFile: string;
begin
  Done := False;
  if Assigned(FOnCreateDfmStream) then
    FOnCreateDfmStream(Self, UnitName, Stream, Done);
  if not Done then
  begin
    if Assigned(FOnGetDfmFileName) then
      FOnGetDfmFileName(Self, UnitName, DfmFile, Done);
    if not Done then
      DfmFile := FindInPath(ChangeFileExt(UnitName, '.dfm'),
        ExtractFilePath(FFileName));
    Done := FileExists(DfmFile);
    if Done then
      Stream := TFileStream.Create(DfmFile, fmOpenRead);
  end;

  if not Done then
    JvInterpreterErrorN(ieDfmNotFound, -1, UnitName);
end;

procedure TJvInterpreterFm.FreeDfmStream(Stream: TStream);
begin
  if Assigned(FOnFreeDfmStream) then
    FOnFreeDfmStream(Self, Stream)
  else
    Stream.Free;
end;

procedure TJvInterpreterFm.LoadForm(AForm: TJvInterpreterForm);
var
  Stream: TStream;
begin
  FForm := AForm;
  Form.FJvInterpreterFm := Self;
  CreateDfmStream(FForm.FUnitName, Stream);
  try
    JvInterpreterReadComponentRes(Stream, Form);
  finally
    FreeDfmStream(Stream);
  end;
  try
    if Assigned(Form.OnCreate) then Form.OnCreate(Form);
  except
    Application.HandleException(Form);
  end;
  if Form.FormStyle <> fsMDIChild then
    Form.Visible := False;
end;

function TJvInterpreterFm.GetValue(Identifer: string; var Value: Variant;
  var Args: TArgs): Boolean;

  function GetFromForm(Form: TJvInterpreterForm): Boolean;
  var
    Com: TComponent;
  begin
    if Cmp(Identifer, 'Self') then
    begin
      Value := O2V(Form);
      Result := True;
      Exit;
    end;
    Com := Form.FindComponent(Identifer);
    if Com = nil then
    begin
      if (LocalVars <> nil) and (LocalVars.FindVar('', Identifer) <> nil) then
      begin
        Result := LocalVars.GetValue(Identifer, Value, Args);
        Exit;
      end;
     { may be TForm method or published property }
      Args.Obj := Form;
      Args.ObjTyp := varObject;
      try
        Result := inherited GetValue(Identifer, Value, Args);
      finally
        Args.Obj := nil;
        Args.ObjTyp := 0;
      end;
    end
    else
    begin
      Value := O2V(Com);
      Result := True;
    end;
  end;

var
  JvInterpreterSrcClass: TJvInterpreterIdentifer;
  JvInterpreterForm: TJvInterpreterForm;
begin
  if (Args.Obj = nil) and (CurInstance is TJvInterpreterForm) then
    Result := GetFromForm(CurInstance as TJvInterpreterForm)
  else
  if (Args.Obj <> nil) and (Args.ObjTyp = varObject) and
     (Args.Obj is TJvInterpreterForm) then
  begin
   { run-time form creation }
    if Cmp(Identifer, 'Create') then
    begin
      JvInterpreterSrcClass := THackAdapter(Adapter).GetSrcClass(
        (Args.Obj as TJvInterpreterForm).FClassIdentifer);
      (Args.Obj as TJvInterpreterForm).FUnitName := JvInterpreterSrcClass.UnitName;  
      LoadForm(Args.Obj as TJvInterpreterForm);
      Value := O2V(Args.Obj);
      Result := True;
      Exit;
    end
    else
      Result := GetFromForm(Args.Obj as TJvInterpreterForm)
  end
  else
    Result := False;

  if Result then Exit;

  { run-time form creation }
  JvInterpreterSrcClass := THackAdapter(Adapter).GetSrcClass(Identifer);
  if JvInterpreterSrcClass <> nil then
  begin
    {$IFDEF COMPILER4_UP}
    JvInterpreterForm := TJvInterpreterForm.CreateNew(Application);
    {$ELSE}
    JvInterpreterForm := TJvInterpreterForm.CreateNew(Application, 0);
    {$ENDIF COMPILER4_UP}
    JvInterpreterForm.FClassIdentifer := Identifer;
    Value := O2V(JvInterpreterForm);
    Result := True;
    Exit;
  end;

  Result := Result or inherited GetValue(Identifer, Value, Args);
end;    { GetValue }

function TJvInterpreterFm.SetValue(Identifer: string; const Value: Variant;
  var Args: TArgs): Boolean;
begin
  if (Args.Obj = nil) and (CurInstance is TJvInterpreterForm) then
  begin
    if (LocalVars <> nil) and (LocalVars.FindVar('', Identifer) <> nil) then
    begin
      Result := LocalVars.SetValue(Identifer, Value, Args);
      Exit;
    end;
   { may be TForm method or published property }
    Args.Obj := CurInstance;
    Args.ObjTyp := varObject;
    try
      Result := inherited SetValue(Identifer, Value, Args);
    finally
      Args.Obj := nil;
      Args.ObjTyp := 0;
    end;
  end
  else
    Result := False;
  Result := Result or inherited SetValue(Identifer, Value, Args);
end;    { SetValue }

function TJvInterpreterFm.GetUnitSource(UnitName: string; var Source: string): boolean;
var
  FN: TFileName;
begin
  if not FInterfaceUses and (UnitSection = usInterface) then
  begin
    Source := 'unit ' + UnitName + '; end.';
    Result := True;
  end
  else
  begin
    Result := inherited GetUnitSource(UnitName, Source);
    if not Result then
    begin
      if ExtractFileExt(UnitName) = '' then
        UnitName := UnitName + '.pas';
      if FileExists(UnitName) then
        FN := UnitName
      else
        FN := FindInPath(ExtractFileName(UnitName), ExtractFilePath(FFileName));
      Result := FileExists(FN);
      if Result then
        Source := LoadTextFile(FN)
    end;
  end;
end;

procedure TJvInterpreterFm.Run;
begin
  inherited Run;
end;

function TJvInterpreterFm.RunForm(const FileName: TFileName): TForm;
begin
  Result := MakeForm(FileName);
  Result.Show;
end;

function TJvInterpreterFm.RunFormModal(const FileName: TFileName): TModalResult;
begin
  with MakeForm(FileName) do
    try
      Result := ShowModal;
    finally { wrap up }
      Free;
    end;    { try/finally }
end;

function TJvInterpreterFm.RunUnit(const FileName: TFileName): Variant;
var
  UnitName: string;
  S: string;
begin
  FFileName := FileName;
  try
    UnitName := ChangeFileExt(ExtractFileName(FFileName), '');
    if not (GetUnitSource(FFileName, S) or GetUnitSource(UnitName, S)) then
      JvInterpreterErrorN(ieUnitNotFound, -1, UnitName);
    Source := S;
  except
    JvInterpreterErrorN(ieUnitNotFound, -1, FFileName);
  end;
  Run;
end;

procedure TJvInterpreterFm.RunReportPreview(const FileName: string);
begin
  JvInterpreterRunReportPreview2(FileName, Self);
end;

function JvInterpreterRunFormModal(const FileName: TFileName): TModalResult;
begin
  with TJvInterpreterFm.Create(Application) do
    try
      Result := RunFormModal(FileName);
    finally { wrap up }
      Free;
    end;    { try/finally }
end;

function JvInterpreterRunForm(const FileName: TFileName): TForm;
begin
  with TJvInterpreterFm.Create(Application) do
  begin
    Result := RunForm(FileName);
    (Result as TJvInterpreterForm).FreeFJvInterpreterFm := True;
  end;
end;

function JvInterpreterMakeForm(const FileName: TFileName): TForm;
begin
  with TJvInterpreterFm.Create(Application) do
  begin
    Result := MakeForm(FileName);
    (Result as TJvInterpreterForm).FreeFJvInterpreterFm := True;
  end;
end;

function JvInterpreterRunUnit(const FileName: TFileName): Variant;
begin
  with TJvInterpreterFm.Create(Application) do
    try
      Result := RunUnit(FileName);
    finally { wrap up }
      Free;
    end;    { try/finally }
end;

{ adapter to self }
{ function JvInterpreterRunFormModal(const FileName: TFileName): TModalResult; }
procedure JvInterpreter_JvInterpreterRunFormModal(var Value: Variant; Args: TArgs);
begin
  Value := JvInterpreterRunFormModal(Args.Values[0]);
end;

{ function JvInterpreterRunForm(const FileName: TFileName): TForm; }
procedure JvInterpreter_JvInterpreterRunForm(var Value: Variant; Args: TArgs);
begin
  Value := O2V(JvInterpreterRunForm(Args.Values[0]));
end;

{ function JvInterpreterMakeForm(const FileName: TFileName): TForm; }
procedure JvInterpreter_JvInterpreterMakeForm(var Value: Variant; Args: TArgs);
begin
  Value := O2V(JvInterpreterMakeForm(Args.Values[0]));
end;

{ function JvInterpreterRunUnit(const FileName: TFileName): Variant }
procedure JvInterpreter_JvInterpreterRunUnit(var Value: Variant; Args: TArgs);
begin
  Value := JvInterpreterRunUnit(Args.Values[0]);
end;

procedure JvInterpreterRunReportPreview(const FileName: string);
begin
  if not Assigned(JvInterpreterRunReportPreviewProc) then
    raise Exception.Create(SNoReportProc);
  JvInterpreterRunReportPreviewProc(FileName);
end;

procedure JvInterpreterRunReportPreview2(const FileName: string; JvInterpreterProgram: TJvInterpreterFm);
begin
  if not Assigned(JvInterpreterRunReportPreview2Proc) then
    raise Exception.Create(SNoReportProc2);
  JvInterpreterRunReportPreview2Proc(FileName, JvInterpreterProgram);
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  with JvInterpreterAdapter do
  begin
    AddFun('JvInterpreterFm', 'JvInterpreterRunFormModal', JvInterpreter_JvInterpreterRunFormModal, 1, [varString], varEmpty);
    AddFun('JvInterpreterFm', 'JvInterpreterRunForm', JvInterpreter_JvInterpreterRunForm, 1, [varString], varEmpty);
    AddFun('JvInterpreterFm', 'JvInterpreterMakeForm', JvInterpreter_JvInterpreterMakeForm, 1, [varString], varEmpty);
    AddFun('JvInterpreterFm', 'JvInterpreterRunUnit', JvInterpreter_JvInterpreterRunUnit, 1, [varString], varEmpty);
  end;    { with }
end;    { RegisterJvInterpreterAdapter }

end.
