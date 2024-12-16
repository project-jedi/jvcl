{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreterFm.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s): Ivan Ravin (ivan_ra)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description : JVCL Interpreter version 2
Component   : form runner for JvInterpreter

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

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

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Controls, Forms,
  JvInterpreter, JvJVCLUtils, JvComponent;

type
  TJvInterpreterGetDfmFileName = procedure(Sender: TObject; UnitName: string;
    var FileName: string; var Done: Boolean) of object;
  TJvInterpreterCreateDfmStream = procedure(Sender: TObject; UnitName: string;
    var Stream: TStream; var Done: Boolean) of object;
  TJvInterpreterFreeDfmStream = procedure(Sender: TObject; Stream: TStream) of object;

  TJvInterpreterFm = class;

  TJvInterpreterForm = class(TJvForm)
  private
    FJvInterpreterFm: TJvInterpreterFm;
    FMethodList: TList;
    FFieldList: TJvInterpreterVarList;
    FFreeJvInterpreterFm: Boolean;
    FClassIdentifier: string;
    FUnitName: string;
    procedure FixupMethods;
  protected
    procedure ReadState(Reader: TReader); override;
    property MethodList: TList read  FMethodList;
    property ClassIdentifier: string read FClassIdentifier;
    {$WARNINGS OFF} // Delphi 2009+ has a class function UnitName
    property UnitName: string read FUnitName;
    {$WARNINGS ON}
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    property JvInterpreterFm: TJvInterpreterFm read FJvInterpreterFm write FJvInterpreterFm;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
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
    function GetValue(const Identifier: string; var Value: Variant;
      var Args: TJvInterpreterArgs): Boolean; override;
    function SetValue(const Identifier: string; const Value: Variant;
      var Args: TJvInterpreterArgs): Boolean; override;
    function GetUnitSource(const UnitName: string; var Source: string): Boolean; override;
    procedure CreateDfmStream(const UnitName: string; var Stream: TStream); dynamic;
    procedure FreeDfmStream(Stream: TStream); dynamic;
  public
    procedure Run; override;
    function MakeForm(const FileName: TFileName): TForm;
    function MakeInheritedForm(F: TJvInterpreterForm; const FileName: TFileName): TForm;
    function RunForm(const FileName: TFileName): TForm;
    function RunFormModal(const FileName: TFileName): TModalResult;
    function RunUnit(const FileName: TFileName): Variant;
    procedure RunReportPreview(const FileName: string);
    property Form: TJvInterpreterForm read FForm;
    property FileName: string read FFileName;
  published
    property OnGetDfmFileName: TJvInterpreterGetDfmFileName read FOnGetDfmFileName write FOnGetDfmFileName;
    property OnCreateDfmStream: TJvInterpreterCreateDfmStream read FOnCreateDfmStream write FOnCreateDfmStream;
    property OnFreeDfmStream: TJvInterpreterFreeDfmStream read FOnFreeDfmStream write FOnFreeDfmStream;
    property InterfaceUses: Boolean read FInterfaceUses write FInterfaceUses default False;
  end;

function JvInterpreterRunFormModal(const AFileName: TFileName): TModalResult;
function JvInterpreterRunForm(const AFileName: TFileName): TForm;
function JvInterpreterMakeForm(const AFileName: TFileName): TForm;
function JvInterpreterRunUnit(const AFileName: TFileName): Variant;
procedure JvInterpreterRunReportPreview(const AFileName: string);
procedure JvInterpreterRunReportPreview2(const AFileName: string; JvInterpreterProgram: TJvInterpreterFm);

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

const
  ieImplementationNotFound = 401;

var
  JvInterpreterRunReportPreviewProc: procedure(const FileName: string);
  JvInterpreterRunReportPreview2Proc: procedure(const FileName: string; JvInterpreterProgram: TJvInterpreterFm);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  TypInfo,
  JvResources, JvTypes, JvJCLUtils;

//=== { TJvInterpreterReader } ===============================================

type
  TJvInterpreterReader = class(TReader)
  protected
    function FindMethod(Root: TComponent; const MethodName: string): Pointer;
      override;
  end;

  TJvInterpreterAdapterAccessProtected = class(TJvInterpreterAdapter);

function TJvInterpreterReader.FindMethod(Root: TComponent; const MethodName: string): Pointer;
var
  Len: Integer;
begin
  // (rom) explicit allocation instead of deprecated NewStr
  Len := Length(MethodName) + 1;
  GetMem(Result, Len * SizeOf(Char));
  Move(PChar(MethodName)^, Result^, Len * SizeOf(Char));
  TJvInterpreterForm(Root).FMethodList.Add(Result);
end;

//=== { TJvInterpreterForm } =================================================

constructor TJvInterpreterForm.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
begin
  FMethodList := TList.Create;
  FFieldList := TJvInterpreterVarList.Create;  // class fields suport
  {$IFDEF DELPHI}
  inherited CreateNew(AOwner);
  {$ELSE}
  inherited CreateNew(AOwner, Dummy);
  {$ENDIF DELPHI}
end;

destructor TJvInterpreterForm.Destroy;
var
  I: Integer;
begin
  for I := 0 to FMethodList.Count - 1 do
    FreeMem(FMethodList[I]);
  FMethodList.Free;
  FFieldList.Free;  // class fields suport
  inherited Destroy;
  if FFreeJvInterpreterFm then
    FJvInterpreterFm.Free;
end;

procedure TJvInterpreterForm.FixupMethods;

  procedure ReadProps(Com: TComponent);
  var
    TypeInf: PTypeInfo;
    TypeData: PTypeData;
    PropList: PPropList;
    NumProps: Word;
    I: Integer;
    F: Integer;
    Method: TMethod;
  begin
    TypeInf := Com.ClassInfo;
    TypeData := GetTypeData(TypeInf);
    NumProps := TypeData^.PropCount;
    GetMem(PropList, NumProps * SizeOf(Pointer));
    try
      GetPropInfos(TypeInf, PropList);
      for I := 0 to NumProps - 1 do
        if PropList^[I].PropType^.Kind = tkMethod then
        begin
          Method := GetMethodProp(Com, PropList^[I]);
          if Method.Data = Self then
          begin
            F := FMethodList.IndexOf(Method.Code);
            if F > -1 then
            begin
              SetMethodProp(Com, PropList^[I],
                TMethod(FJvInterpreterFm.NewEvent(FUnitName,
                PChar(FMethodList[F]),
                   {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropList^[I]^.PropType^.Name),
                   Self,
                   {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropList^[I]^.Name))));
            end;
          end;
        end;
    finally
      FreeMem(PropList);
    end;
  end;

var
  I: Integer;
begin
  if FJvInterpreterFm = nil then
    Exit; {+RWare}
  ReadProps(Self);
  for I := 0 to ComponentCount - 1 do
    ReadProps(Components[I]);
end;

procedure TJvInterpreterForm.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  FixupMethods;
end;

function JvInterpreterReadComponentRes(var Stream: TStream;
  Instance: TComponent): TComponent;
var
  JvInterpreterReader: TJvInterpreterReader;
  TmpStream: TMemoryStream;
begin
  if TestStreamFormat(Stream) = sofText then
  begin
    TmpStream := TMemoryStream.Create;
    ObjectTextToResource(Stream, TmpStream);
    Stream.Free;
    Stream := TmpStream;
    Stream.Position := 0;
  end;

  Stream.ReadResHeader;
  JvInterpreterReader := TJvInterpreterReader.Create(Stream, 4096);
  try
    Result := JvInterpreterReader.ReadRootComponent(Instance);
  finally
    JvInterpreterReader.Free;
  end;
end;

//=== { TJvInterpreterFm } ===================================================

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
  FForm := TJvInterpreterForm.CreateNew(Application);
  FForm.FUnitName := UnitName;
  LoadForm(FForm);
  Result := FForm;
end; { MakeForm }

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
end;

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
  SrcClass: TJvInterpreterIdentifier;                // Class Fields support
  i: integer;
begin
  FForm := AForm;
  Form.FJvInterpreterFm := Self;
  CreateDfmStream(FForm.FUnitName, Stream);
  try
    JvInterpreterReadComponentRes(Stream, Form);
  finally
    FreeDfmStream(Stream);
  end;
  // find form class
  if AForm.FClassIdentifier = '' then
    for i:=0 to Adapter.SrcClassList.Count-1 do
      if cmp(TJvInterpreterIdentifier(Adapter.SrcClassList[i]).UnitName,FForm.FUnitName) then
      begin
        FForm.FClassIdentifier := TJvInterpreterIdentifier(Adapter.SrcClassList[i]).Identifier;
        Break;
      end;
  // Class Fields support begin
  // copy form fields from pattern
  SrcClass := TJvInterpreterAdapterAccessProtected(Adapter).GetSrcClass(
    AForm.FClassIdentifier);
  if assigned(SrcClass) then
    AForm.FFieldList.Assign(TJvInterpreterClass(SrcClass).ClassFields);
  // Class Fields support end
  try
    if Assigned(Form.OnCreate) then
      Form.OnCreate(Form);
  except
    Application.HandleException(Form);
  end;
  if Form.FormStyle <> fsMDIChild then
    Form.Visible := False;
end;

function TJvInterpreterFm.GetValue(const Identifier: string; var Value: Variant;
  var Args: TJvInterpreterArgs): Boolean;
var
  JvInterpreterSrcClass: TJvInterpreterIdentifier;
  JvInterpreterForm: TJvInterpreterForm;
  LocalArgs: TJvInterpreterArgs;

  function GetFromForm(Form: TJvInterpreterForm): Boolean;
  var
    Com: TComponent;
  begin
    if Cmp(Identifier, 'Self') then
    begin
      Value := O2V(Form);
      Result := True;
      Exit;
    end;
    Com := Form.FindComponent(Identifier);
    if Com = nil then
    begin
      if (LocalVars <> nil) and (LocalVars.FindVar('', Identifier) <> nil) then
      begin
        Result := LocalVars.GetValue(Identifier, Value, Args);
        Exit;
      end;
      // Class Fields support begin
      with Form.FFieldList do
      if FindVar('', Identifier) <> nil then
      begin
        Args.Obj:=nil;
        Result := GetValue(Identifier, Value, Args);
        Exit;
      end;
      // Class Fields support end
      { may be TForm method or published property }
      Args.Obj := Form;
      Args.ObjTyp := varObject;
      try
        Result := inherited GetValue(Identifier, Value, Args);
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

begin
  if (Args.Obj = nil) and (CurInstance is TJvInterpreterForm) then
    Result := GetFromForm(CurInstance as TJvInterpreterForm)
  else
  if (Args.Obj <> nil) and (Args.ObjTyp = varObject) and
    (Args.Obj is TJvInterpreterForm) then
  begin
    { run-time form creation }
    if Cmp(Identifier, 'Create') then
    begin
      // setting form's Owner from expression 'create(newOwner)'
      // when Identifier = 'Create' then Token = 'newOwner'
      JvInterpreterForm := Args.Obj as TJvInterpreterForm;
      LocalArgs := TJvInterpreterArgs.Create;
      try
        LocalArgs.Obj:=Args.Obj;
        LocalArgs.ObjTyp:=Args.ObjTyp;
        GetValue(Token, Value, LocalArgs);
      finally
        LocalArgs.Free;
      end;
      if V2O(Value)<>JvInterpreterForm.Owner then begin
        if JvInterpreterForm.Owner<>nil then
          JvInterpreterForm.Owner.RemoveComponent(JvInterpreterForm);
        if V2O(Value)<>nil then
          TComponent(V2O(Value)).InsertComponent(JvInterpreterForm);
      end;
      JvInterpreterSrcClass := TJvInterpreterAdapterAccessProtected(Adapter).GetSrcClass(
        JvInterpreterForm.FClassIdentifier);
      JvInterpreterForm.FUnitName := JvInterpreterSrcClass.UnitName;
      LoadForm(JvInterpreterForm);
      Value := O2V(Args.Obj);
      Result := True;
      Exit;
    end
    else
      Result := GetFromForm(Args.Obj as TJvInterpreterForm)
  end
  else
    Result := False;

  if Result then
    Exit;

  { run-time form creation }
  JvInterpreterSrcClass := TJvInterpreterAdapterAccessProtected(Adapter).GetSrcClass(Identifier);
  if JvInterpreterSrcClass <> nil then
  begin
    JvInterpreterForm := TJvInterpreterForm.CreateNew(Application);
    JvInterpreterForm.FClassIdentifier := Identifier;
    Value := O2V(JvInterpreterForm);
    Result := True;
    Exit;
  end;

  Result := Result or inherited GetValue(Identifier, Value, Args);
end;

function TJvInterpreterFm.SetValue(const Identifier: string; const Value: Variant;
  var Args: TJvInterpreterArgs): Boolean;
  // Class Fields support begin
var
  JvInterpreterForm: TJvInterpreterForm;

  function SetFormValue(Form: TJvInterpreterForm): Boolean;
  begin
    Result := False;
    with Form.FFieldList do
      if FindVar('', Identifier) <> nil then begin
        Args.Obj := nil;
        Result := SetValue(Identifier, Value, Args);
      end;
  end;
  // Class Fields support end

begin
  if (Args.Obj = nil) and (CurInstance is TJvInterpreterForm) then
  begin
    if (LocalVars <> nil) and (LocalVars.FindVar('', Identifier) <> nil) then
    begin
      Result := LocalVars.SetValue(Identifier, Value, Args);
      Exit;
    end;
    // Class Fields support begin
    { may be TForm field }
    Result := SetFormValue(TJvInterpreterForm(CurInstance));
    if not Result then
    begin
    // Class Fields support end
      { may be TForm method or published property }
      Args.Obj := CurInstance;
      Args.ObjTyp := varObject;
      try
        Result := inherited SetValue(Identifier, Value, Args);
      finally
        Args.Obj := nil;
        Args.ObjTyp := 0;
      end;
    end;
  end
  // Class Fields support begin
  else
  if (Args.Obj <> nil) and (Args.ObjTyp = varObject) and
     (Args.Obj is TJvInterpreterForm) then
  begin
    JvInterpreterForm := TJvInterpreterForm(Args.Obj);
    try
      Args.Obj := nil;
      Result := SetFormValue(JvInterpreterForm);
    finally
      Args.Obj := JvInterpreterForm;
    end;
  end
  // Class Fields support end
  else
    Result := False;
  Result := Result or inherited SetValue(Identifier, Value, Args);
end;

function TJvInterpreterFm.GetUnitSource(const UnitName: string; var Source: string): Boolean;
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
        FN := UnitName + '.pas'
      else
        FN := UnitName;
      Result := FileExists(FN);
      if not Result then
      begin
        FN := FindInPath(ExtractFileName(FN), ExtractFilePath(FFileName));
        Result := FileExists(FN);
      end;
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
  finally
    Free;
  end;
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

function JvInterpreterRunFormModal(const AFileName: TFileName): TModalResult;
var
  TmpInterpreterFm: TJvInterpreterFm;
begin
  TmpInterpreterFm := TJvInterpreterFm.Create(Application);
  try
    Result := TmpInterpreterFm.RunFormModal(AFileName);
  finally
    TmpInterpreterFm.Free;
  end;
end;

function JvInterpreterRunForm(const AFileName: TFileName): TForm;
var
  TmpInterpreterFm: TJvInterpreterFm;
begin
  TmpInterpreterFm := TJvInterpreterFm.Create(Application);
  begin
    Result := TmpInterpreterFm.RunForm(AFileName);
    (Result as TJvInterpreterForm).FFreeJvInterpreterFm := True;
  end;
end;

function JvInterpreterMakeForm(const AFileName: TFileName): TForm;
var
  TmpInterpreterFm: TJvInterpreterFm;
begin
  TmpInterpreterFm := TJvInterpreterFm.Create(Application);
  begin
    Result := TmpInterpreterFm.MakeForm(AFileName);
    (Result as TJvInterpreterForm).FFreeJvInterpreterFm := True;
  end;
end;

function JvInterpreterRunUnit(const AFileName: TFileName): Variant;
var
  TmpInterpreterFm: TJvInterpreterFm;
begin
  TmpInterpreterFm := TJvInterpreterFm.Create(Application);
  try
    Result := TmpInterpreterFm.RunUnit(AFileName);
  finally
    TmpInterpreterFm.Free;
  end;
end;

{ adapter to self }
{ function JvInterpreterRunFormModal(const FileName: TFileName): TModalResult; }

procedure JvInterpreter_JvInterpreterRunFormModal(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := JvInterpreterRunFormModal(Args.Values[0]);
end;

{ function JvInterpreterRunForm(const FileName: TFileName): TForm; }

procedure JvInterpreter_JvInterpreterRunForm(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(JvInterpreterRunForm(Args.Values[0]));
end;

{ function JvInterpreterMakeForm(const FileName: TFileName): TForm; }

procedure JvInterpreter_JvInterpreterMakeForm(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(JvInterpreterMakeForm(Args.Values[0]));
end;

{ function JvInterpreterRunUnit(const FileName: TFileName): Variant }

procedure JvInterpreter_JvInterpreterRunUnit(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := JvInterpreterRunUnit(Args.Values[0]);
end;

procedure JvInterpreterRunReportPreview(const AFileName: string);
begin
  if not Assigned(JvInterpreterRunReportPreviewProc) then
    raise EJVCLException.CreateRes(@RsENoReportProc);
  JvInterpreterRunReportPreviewProc(AFileName);
end;

procedure JvInterpreterRunReportPreview2(const AFileName: string; JvInterpreterProgram: TJvInterpreterFm);
begin
  if not Assigned(JvInterpreterRunReportPreview2Proc) then
    raise EJVCLException.CreateRes(@RsENoReportProc2);
  JvInterpreterRunReportPreview2Proc(AFileName, JvInterpreterProgram);
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cJvInterpreterFm = 'JvInterpreterFm';
begin
  with JvInterpreterAdapter do
  begin
    AddFunction(cJvInterpreterFm, 'JvInterpreterRunFormModal', JvInterpreter_JvInterpreterRunFormModal, 1, [varString],
      varEmpty);
    AddFunction(cJvInterpreterFm, 'JvInterpreterRunForm', JvInterpreter_JvInterpreterRunForm, 1, [varString], varEmpty);
    AddFunction(cJvInterpreterFm, 'JvInterpreterMakeForm', JvInterpreter_JvInterpreterMakeForm, 1, [varString], varEmpty);
    AddFunction(cJvInterpreterFm, 'JvInterpreterRunUnit', JvInterpreter_JvInterpreterRunUnit, 1, [varString], varEmpty);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
