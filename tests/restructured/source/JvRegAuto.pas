{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRegAuto.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : JvRegAuto
description : registry and ini-file storage for properties of other components 

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvRegAuto;

interface

uses
{$IFDEF COMPLIB_VCL}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  TypInfo, Registry, IniFiles;
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
  SysUtils, Classes, QForms,
  //Graphics, Controls, Forms,
  TypInfo, IniFiles;
{$ENDIF COMPLIB_CLX}

type

  TJvIniStrings = class;
  TJvMyIniFile = class;
  TRegAutoOperation = (roBeforeLoad, roAfterLoad, roBeforeSave, roAfterSave);
  TRegAutoEvent = procedure (Sender : TObject; Operation : TRegAutoOperation) of object;
  TStorageMedia = (raRegistry, raIniFile, raIniStrings);

  TJvRegAuto = class(TComponent)
  private
    ObjProp : string;
    TypeInf : PTypeInfo;
    PropInf : PPropInfo;
    PropTyp : TTypeKind;
    Obj : TObject;
    ComponentName, PropertyName : string;
    FLoaded : boolean;
    FProps : TStrings;
    FAutoMode : boolean;
    FSaveWindowPlace : boolean;
    CurSection: string;
    CurKey: string;

    FStorage: TStorageMedia;

   {$IFDEF COMPLIB_VCL}
    FRegPath : string;
   {$ENDIF COMPLIB_VCL}
    FIniFile : string;
    FIniStrings : TStrings;
    FGlobalSettings : boolean;
    FSection: string;

    FormOnCreate    : TNotifyEvent;
    FormOnDestroy   : TNotifyEvent;

    FNotifiers : TList;

    FBeforeLoad : TNotifyEvent;
    FBeforeSave : TNotifyEvent;
    FAfterLoad  : TNotifyEvent;
    FAfterSave  : TNotifyEvent;

    OldIniFile : string;
   {$IFDEF COMPLIB_VCL}
    OldRegPath : string;
   {$ENDIF COMPLIB_VCL}

    procedure LoadPropInf(lObjProp : string);
    function GetOrdPrp : longint;
    procedure SetOrdPrp(Value : longint);
    function GetStrPrp : string;
    procedure SetStrPrp(Value : string);
    function GetFloatPrp : extended;
    procedure SetFloatPrp(Value : extended);
    {************ Для Property ************}
    procedure SetFProps(lProps : TStrings);
   {$IFDEF COMPLIB_VCL}
    procedure SetSaveWindowPlace(F : boolean);
   {$ENDIF COMPLIB_VCL}
    procedure SetIniStrings(AIniStrings : TStrings);
   {$IFDEF COMPILER4_UP}
    function GetUse(Index: TStorageMedia): Boolean;
    procedure SetUse(Index: TStorageMedia; Value: Boolean);
   {$ELSE}
    function GetUse(Index: Integer): Boolean;
    procedure SetUse(Index: Integer; Value: Boolean);
   {$ENDIF COMPILER4_UP}
    {############ Для Property ############}
    procedure NewFormOnCreate(Sender : TObject);
    procedure NewFormOnDestroy(Sender : TObject);
    procedure GenerateRegistryName;
    procedure ReadUseRegProperty(Reader: TReader);
    procedure ReadUseIniProperty(Reader: TReader);
    procedure ReadUseStrProperty(Reader: TReader);
  protected
   {$IFDEF COMPLIB_VCL}
    Reg : TRegIniFile;
   {$ENDIF COMPLIB_VCL}
    Ini : TJvMyIniFile;
    Str : TJvIniStrings;
    procedure CreateFile;
    procedure DestroyFile;
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save;
    procedure Load;
   {$IFDEF COMPLIB_VCL}
    procedure SaveWindowPlacement;
    procedure LoadWindowPlacement;
   {$ENDIF COMPLIB_VCL}

   {$IFDEF COMPLIB_VCL}
    function ReadRootString(const Section, Ident, Default: string): string;
    function ReadRootInteger(const Section, Ident: string; Default: Longint): Longint;
    procedure WriteRootString(const Section, Ident, Value: string);
    procedure WriteRootInteger(const Section, Ident: string; Value: Longint);
   {$ENDIF COMPLIB_VCL}

    function ReadString(const Section, Ident, Default: string): string;
    procedure WriteString(const Section, Ident, Value: String);
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint;
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    function ReadFloat(const Section, Ident: string; Default: Double): Double;
    procedure WriteFloat(const Section, Ident: string; Value: Double);
    procedure ReadStrings(const Section, Ident : string; Strings : TStrings);
    procedure WriteStrings(const Section, Ident: string; Value: TStrings);
    procedure ReadSection(const Section : string; Ss : TStrings);
    procedure ReadSectionValues(const Section: string; Ss: TStrings);
    procedure ReadSections(Ss : TStrings);
    procedure ReadWholeSection(const Section : string; Ss : TStrings);
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section, Ident: string);

    // next three properties are only for compatibility with
    // previous TJvRegAuto versions, don't use them in new programs.
   {$IFDEF COMPILER4_UP}
    property UseReg: Boolean index raRegistry read GetUse write SetUse;
    property UseIni: Boolean index raIniFile read GetUse write SetUse;
    property UseStr: Boolean index raIniStrings read GetUse write SetUse;
   {$ELSE}
    property UseReg: Boolean index 0 read GetUse write SetUse;
    property UseIni: Boolean index 1 read GetUse write SetUse;
    property UseStr: Boolean index 2 read GetUse write SetUse;
   {$ENDIF COMPILER4_UP}

    procedure AddNotify(ANotify : TRegAutoEvent);
    procedure RemoveNotify(ANotify : TRegAutoEvent);
    function GetFullIniFileName: String;
  published
    // Путь в реестре
   {$IFDEF COMPLIB_VCL}
    property RegPath : string read FRegPath write FRegPath;
    property Storage: TStorageMedia read FStorage write FStorage default raRegistry;
   {$ENDIF COMPLIB_VCL}
   {$IFDEF COMPLIB_CLX}
    property Storage: TStorageMedia read FStorage write FStorage default raIniFile;
   {$ENDIF COMPLIB_CLX}
    // Имя ini-файла
    property IniFile : string read FIniFile write FIniFile;
    //
    property IniStrings : TStrings read FIniStrings write SetIniStrings;

    // Список сохраняемых свойств
    property Props : TStrings read FProps write SetFProps;
    property AutoMode : boolean read FAutoMode write FAutoMode default true;
    // Сохранять размер и положение окна
   {$IFDEF COMPLIB_VCL}
    property SaveWindowPlace : boolean read FSaveWindowPlace write SetSaveWindowPlace default False;
   {$ENDIF COMPLIB_VCL}
    property BeforeLoad : TNotifyEvent read FBeforeLoad write FBeforeLoad;
    property AfterLoad  : TNotifyEvent read FAfterLoad  write FAfterLoad;
    property BeforeSave : TNotifyEvent read FBeforeSave write FBeforeSave;
    property AfterSave  : TNotifyEvent read FAfterSave  write FAfterSave;
    property GlobalSettings : boolean read FGlobalSettings write FGlobalSettings default true;
    property Section: string read FSection write FSection;
  end;

  TJvIniStrings = class
  private
    FStrings : TStrings;
  public
    constructor Create(AStrings : TStrings);

    function ReadString(const Section, Ident, Default: string): string;
    procedure WriteString(const Section, Ident, Value: String);
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint;
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    function ReadFloat(const Section, Ident: string; Default: Double): Double;
    procedure WriteFloat(const Section, Ident: string; Value: Double);
    function ReadSection(const Section : string; Ss : TStrings) : boolean;
    procedure ReadSectionValues(const Section: string; Ss: TStrings);
    procedure ReadWholeSection(const Section : string; Ss : TStrings);
    procedure ReadSections(Ss : TStrings);
  end;

  TJvMyIniFile = class(TIniFile)
  public
    procedure ReadWholeSection(const Section : string; Ss : TStrings);
   {$IFNDEF COMPILER35_Up}
    function ReadFloat(const Section, Ident: string; Default: Double): Double;
    procedure WriteFloat(const Section, Ident: string; Value: Double);
   {$ENDIF COMPILER35_Up}
  end;

  EJvRegAutoError  = class(Exception);

var
  GlobalIniFile : string = ''; {if <> '', used by all RegAutos}
  GlobalRegPath : string = ''; {if <> '', used by all RegAutos}

implementation

uses JvStrUtil, JvDsgnIntf;


function GetUserHome: string;
begin
 {$IFDEF MSWINDOWS}
 {$IFDEF COMPILER6_UP}
  Result := GetEnvironmentVariable('USERPROFILE');
 {$ELSE}
  SetLength(Result, 1024);
  SetLength(Result, GetEnvironmentVariable('USERPROFILE', PChar(Result), 1024));
 {$ENDIF}
 {$ENDIF MSWINDOWS}
 {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME');
 {$ENDIF LINUX}
end;

{$IFDEF COMPILER2}
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,1
        SHR     ECX,1
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;
{$ENDIF COMPILER2}

constructor TJvRegAuto.Create(AOwner: TComponent);
var
  ProjName : string;
begin
  inherited Create(AOwner);
  FNotifiers := TList.Create;
 {$IFDEF COMPLIB_VCL}
  FStorage := raRegistry;
 {$ENDIF COMPLIB_VCL}
 {$IFDEF COMPLIB_CLX}
  FStorage := raIniFile;
 {$ENDIF COMPLIB_CLX}
  FGlobalSettings := true;
  FProps := TStringList.Create;
  FIniStrings := TStringList.Create;
  ProjName := '';
  if (csDesigning in ComponentState) and Assigned(GetProjectNameProc) then
  begin
    ProjName := GetProjectNameProc;
    ProjName := ExtractFileName(ProjName);
    ProjName := ChangeFileExt(ProjName, '');
    if ProjName = '' then ProjName := 'NONAME';
   {$IFDEF COMPLIB_VCL}
    with TRegIniFile.Create('') do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if Win32Platform = VER_PLATFORM_WIN32_NT then
          FRegPath := 'Software\' + ReadString('Software\Microsoft\Windows NT\CurrentVersion', 'RegisteredOrganization', '') +'\'+ ProjName
        else
          FRegPath := 'Software\' + ReadString('Software\Microsoft\Windows\CurrentVersion', 'RegisteredOrganization', '') +'\'+ ProjName;
      finally
        Reg.Free;
      end;
   {$ENDIF COMPLIB_VCL}
    FIniFile := '$HOME/.' + ProjName;
  end else
  begin
   {$IFDEF COMPLIB_VCL}
    FRegPath := 'Software\Unknown Delphi Application';
   {$ENDIF COMPLIB_VCL}
  end;
  FAutoMode := true;
  FSaveWindowPlace := false;
end;

destructor TJvRegAuto.Destroy;
begin
  FProps.Free;
  FIniStrings.Free;
  FNotifiers.Free;
  inherited Destroy;
end;

procedure TJvRegAuto.Loaded;

  function EqualAddr(Addr1, Addr2: TNotifyEvent): Boolean;
  begin
    Result := CompareMem(@TMethod(Addr1), @TMethod(Addr2), 8);
  end;
             
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) and FAutoMode then
    if (Owner is TForm) and
        not EqualAddr((Owner as TForm).OnCreate, NewFormOnCreate) then
    begin
      FormOnCreate := (Owner as TForm).OnCreate;
      (Owner as TForm).OnCreate := NewFormOnCreate;
      FormOnDestroy := (Owner as TForm).OnDestroy;
      (Owner as TForm).OnDestroy := NewFormOnDestroy;
    end else if (Owner is TDataModule) and
        not EqualAddr((Owner as TDataModule).OnCreate, NewFormOnCreate) then
    begin
      FormOnCreate := (Owner as TDataModule).OnCreate;
      (Owner as TDataModule).OnCreate := NewFormOnCreate;
      FormOnDestroy := (Owner as TDataModule).OnDestroy;
      (Owner as TDataModule).OnDestroy := NewFormOnDestroy;
    end;
end;    { Loaded }

procedure TJvRegAuto.NewFormOnCreate(Sender : TObject);
begin
  if Assigned(FormOnCreate) then FormOnCreate(Self);
  if AutoMode then Load;
end;

procedure TJvRegAuto.NewFormOnDestroy(Sender : TObject);
begin
  if AutoMode then Save;
  if Assigned(FormOnDestroy) then FormOnDestroy(Self);
end;

procedure TJvRegAuto.LoadPropInf(lObjProp : string);
var
  PropName : string;
  i        : integer;
  Last     : boolean;
begin
  FLoaded := false;
  ObjProp := lObjProp;
  if Length(ObjProp) < 2 then exit;
  ComponentName := Copy(ObjProp, 1, Pos('.', ObjProp)-1);
  PropertyName  := Copy(ObjProp, Pos('.', ObjProp)+1, 255);
  if ComponentName = Owner.Name then begin
    ComponentName := '';
    ObjProp := Copy(ObjProp, Pos('.', ObjProp), 255);
  end;
  if ComponentName = '' then Obj := Owner
  else Obj := Owner.FindComponent(ComponentName);
  if Obj = nil then exit;
  
  // Ищем вложенные свойства
  i := Pos('.', ObjProp);
  Last := false;
  while not Last do begin
    PropName := '';
    inc(i);
    while i <= Length(ObjProp) do begin
      if ObjProp[i] = '.' then break;
      PropName := PropName + ObjProp[i];
      inc(i);
    end;
    Last := not ((Length(ObjProp) >= i) and (ObjProp[i] = '.'));
    TypeInf := Obj.ClassInfo;
    PropInf := GetPropInfo(TypeInf, PropName);
    if PropInf = nil then exit;
    PropTyp := PropInf^.PropType^.Kind;
    if Obj = nil then exit;
    if PropTyp = tkClass then begin
      FLoaded := true;
      Obj := TObject(GetOrdPrp);
      FLoaded := false;
    end
    else break; // Loop End
  end;
  FLoaded := true;
end;

procedure TJvRegAuto.GenerateRegistryName;
begin
  if FSection = '' then
    CurSection := Owner.Name
  else
    CurSection := '';
  CurKey  := ObjProp;
end;

function TJvRegAuto.GetOrdPrp : longint;
begin
  Result := 0;
  case PropTyp of
    tkInteger,
    tkChar,
    tkWChar,
    tkClass,
    tkEnumeration :
      if FLoaded then Result:= GetOrdProp(Obj, PropInf);
  end;
end;

procedure TJvRegAuto.SetOrdPrp(Value : longint);
begin
  case PropTyp of
    tkInteger,
    tkChar,
    tkWChar,
    tkEnumeration :
      if FLoaded then SetOrdProp(Obj, PropInf, Value);
  end;
end;

function TJvRegAuto.GetStrPrp : string;
begin
  Result := '';
  case PropTyp of
    tkString,
    tkLString{$IFDEF COMPILER3_UP},
    tkWString{$ENDIF COMPILER3_UP} :
      if FLoaded then Result:= GetStrProp(Obj, PropInf);
  end;
end;

procedure TJvRegAuto.SetStrPrp(Value : string);
begin
  case PropTyp of
    tkString,
    tkLString{$IFDEF COMPILER3_UP},
    tkWString{$ENDIF COMPILER3_UP} :
      if FLoaded then SetStrProp(Obj, PropInf, Value);
  end;
end;

function TJvRegAuto.GetFloatPrp : extended;
begin
  Result := 0;
  case PropTyp of
    tkFloat :
      if FLoaded then Result:= GetFloatProp(Obj, PropInf);
  end;
end;

procedure TJvRegAuto.SetFloatPrp(Value : extended);
begin
  case PropTyp of
    tkFloat :
      if FLoaded then SetFloatProp(Obj, PropInf, Value);
  end;
end;

procedure TJvRegAuto.Save;

  procedure SaveOrdPrp;
  var
    Value : longint;
  begin
    Value := GetOrdPrp;
    WriteInteger(CurSection, CurKey, Value);
  end;
  procedure SaveFloatPrp;
  var
    Value : extended;
  begin
    Value := GetFloatPrp;
    WriteFloat(CurSection, CurKey, Value);
  end;
  procedure SaveStrPrp;
  var
    Value : string;
  begin
    Value := GetStrPrp;
    WriteString(CurSection, CurKey, Value);
  end;

var
  i : integer;
begin
  for i := 0 to FNotifiers.Count - 1 do
    TRegAutoEvent(FNotifiers[i]^)(Self, roBeforeSave);
  if Assigned(FBeforeSave) then FBeforeSave(Self);
 try
  for i:= 0 to FProps.Count - 1 do begin
    LoadPropInf(FProps[i]);
    if not FLoaded then continue;
    GenerateRegistryName;
    case PropTyp of
      tkInteger,
      tkChar,
      tkWChar,
      tkEnumeration : SaveOrdPrp;
      tkFloat       : SaveFloatPrp;
      tkString,
      tkLString{$IFDEF COMPILER3_UP},
      tkWString{$ENDIF COMPILER3_UP}    : SaveStrPrp;
    end;
  end;
 {$IFDEF COMPLIB_VCL}
  if SaveWindowPlace then SaveWindowPlacement;
 {$ENDIF COMPLIB_VCL}
 except
   raise EJvRegAutoError .Create('Could not save property ' + ObjProp);
 end;
  if Assigned(FAfterSave) then FAfterSave(Self);
  for i := 0 to FNotifiers.Count - 1 do
    TRegAutoEvent(FNotifiers[i]^)(Self, roAfterSave);
end;

procedure TJvRegAuto.Load;

  procedure LoadOrdPrp;
  var
    Value : longint;
  begin
    Value := GetOrdPrp;
    Value := ReadInteger(CurSection, CurKey, Value);
    SetOrdPrp(Value);
  end;
  procedure LoadFloatPrp;
  var
    Value : extended;
  begin
    Value := GetFloatPrp;
    Value := ReadFloat(CurSection, CurKey, Value);
    SetFloatPrp(Value);
  end;
  procedure LoadStrPrp;
  var
    Value : string;
  begin
    Value := GetStrPrp;
    Value := ReadString(CurSection, CurKey, Value);
    SetStrPrp(Value);
  end;

var
  i : integer;
begin
  for i := 0 to FNotifiers.Count - 1 do
    TRegAutoEvent(FNotifiers[i]^)(Self, roBeforeLoad);
  if Assigned(FBeforeLoad) then FBeforeLoad(Self);
 try
 {$IFDEF COMPLIB_VCL}
  if SaveWindowPlace then LoadWindowPlacement;
 {$ENDIF COMPLIB_VCL}
  for i:= 0 to FProps.Count - 1 do begin
    LoadPropInf(FProps[i]);
    if not FLoaded then continue;
    GenerateRegistryName;
    case PropTyp of
      tkInteger,
      tkChar,
      tkWChar,
      tkEnumeration : LoadOrdPrp;
      tkFloat       : LoadFloatPrp;
      tkString,
      tkLString{$IFDEF COMPILER3_UP},
      tkWString{$ENDIF COMPILER3_UP}     : LoadStrPrp;
    end;
  end;
 except
   on E : Exception do
     raise EJvRegAutoError .Create('Could not load property: ' + E.Message);
 end;
  if Assigned(FAfterLoad) then FAfterLoad(Self);
  for i := 0 to FNotifiers.Count - 1 do
    TRegAutoEvent(FNotifiers[i]^)(Self, roAfterLoad);
end;

{$IFDEF COMPLIB_VCL}
procedure TJvRegAuto.LoadWindowPlacement;
var
  W : TWINDOWPLACEMENT;
  Form : TForm;
const
  Vis : array[boolean] of integer = (SW_HIDE, SW_SHOW);
begin
  Form := Owner as TForm;
  W.length := sizeof(TWINDOWPLACEMENT);
  GetWindowPlacement(Form.Handle, @W);
  W.showCmd := Vis[Form.Visible];
  GenerateRegistryName;
  with W.rcNormalPosition do begin
    Left   := ReadInteger(CurSection, '.Left', Form.Left);
    Top    := ReadInteger(CurSection, '.Top', Form.Top);
    if Form.BorderStyle in [bsSizeable, bsSizeToolWin] then
    begin
      Right  := ReadInteger(CurSection, '.Right', Right);
      Bottom := ReadInteger(CurSection, '.Bottom', Bottom);
    end else
    begin
      Right  := Left + Form.Width;
      Bottom := Top + Form.Height;
    end;
  end;
  SetWindowPlacement(Form.Handle, @W);
  Form.WindowState := TWindowState(ReadInteger(CurSection, '.WindowState', integer(Form.WindowState)));
end;

procedure TJvRegAuto.SaveWindowPlacement;
var
  W : TWINDOWPLACEMENT;
  Form : TForm;
begin
  Form := Owner as TForm;
  W.length := sizeof(TWINDOWPLACEMENT);
  GetWindowPlacement(Form.Handle, @W);
  GenerateRegistryName;
  with W.rcNormalPosition do begin
    WriteInteger(CurSection, '.Left', Left);
    WriteInteger(CurSection, '.Top', Top);
    WriteInteger(CurSection, '.Right', Right);
    WriteInteger(CurSection, '.Bottom', Bottom);
  end;
  WriteInteger(CurSection, '.WindowState', integer(Form.WindowState));
end;
{$ENDIF COMPLIB_VCL}

procedure TJvRegAuto.SetFProps(lProps : TStrings);
begin
  FProps.Assign(lProps);
end;

{$IFDEF COMPLIB_VCL}
function TJvRegAuto.ReadRootString(const Section, Ident, Default: string): string;
var
  RegIni1 : TRegIniFile;
begin
  RegIni1 := TRegIniFile.Create('');
  Result := RegIni1.ReadString(Section, Ident, Default);
  RegIni1.Free;
end;

function TJvRegAuto.ReadRootInteger(const Section, Ident: string; Default: Longint): Longint;
var
  RegIni1 : TRegIniFile;
begin
  RegIni1 := TRegIniFile.Create('');
  Result := RegIni1.ReadInteger(Section, Ident, Default);
  RegIni1.Free;
end;

procedure TJvRegAuto.WriteRootString(const Section, Ident, Value: string);
var
  RegIni1 : TRegIniFile;
begin
  RegIni1 := TRegIniFile.Create('');
  RegIni1.WriteString(Section, Ident, Value);
  RegIni1.Free;
end;

procedure TJvRegAuto.WriteRootInteger(const Section, Ident: string; Value: Longint);
var
  RegIni1 : TRegIniFile;
begin
  RegIni1 := TRegIniFile.Create('');
  RegIni1.WriteInteger(Section, Ident, Value);
  RegIni1.Free;
end;

procedure TJvRegAuto.SetSaveWindowPlace(F : boolean);
begin
  if Owner is TWinControl then FSaveWindowPlace := F;
end;
{$ENDIF COMPLIB_VCL}

procedure TJvRegAuto.SetIniStrings(AIniStrings : TStrings);
begin
  IniStrings.Assign(AIniStrings);
end;

{$IFDEF COMPILER4_UP}
function TJvRegAuto.GetUse(Index: TStorageMedia): Boolean;
begin
  Result := FStorage = Index;
end;

procedure TJvRegAuto.SetUse(Index: TStorageMedia; Value: Boolean);
begin
  FStorage := Index;
end;

{$ELSE}

function TJvRegAuto.GetUse(Index: Integer): Boolean;
begin
  Result := FStorage = TStorageMedia(Index);
end;

procedure TJvRegAuto.SetUse(Index: Integer; Value: Boolean);
begin
  FStorage := TStorageMedia(Index);
end;
{$ENDIF COMPILER4_UP}

{**************************************************}
function TJvRegAuto.GetFullIniFileName: String;
begin
  Result := ReplaceString(FIniFile, '$HOME', GetUserHome);
  // make path relative to executable
 {$IFDEF MSWINDOWS}
  if not ((Length(Result) > 2) and (Result[2] = ':')) then
    Result := ExtractFilePath(Application.ExeName) + Result;
 {$ENDIF MSWINDOWS}
 {$IFDEF LINUX}
  if not ((Length(Result) > 0) and (Result[1] = '/')) then
    Result := ExtractFilePath(Application.ExeName) + Result;
 {$ENDIF LINUX}
end;

procedure TJvRegAuto.CreateFile;
begin
  OldIniFile := FIniFile;
 {$IFDEF COMPLIB_VCL}
  OldRegPath := FRegPath;
 {$ENDIF COMPLIB_VCL}
  if FGlobalSettings then begin
    if GlobalIniFile <> '' then FIniFile := GlobalIniFile;
   {$IFDEF COMPLIB_VCL}
    if GlobalRegPath <> '' then FRegPath := GlobalRegPath;
   {$ENDIF COMPLIB_VCL}
  end;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
      Reg := TRegIniFile.Create(FRegPath);
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      begin
        Ini := TJvMyIniFile.Create(GetFullIniFileName());
      end;
    raIniStrings:
      Str := TJvIniStrings.Create(FIniStrings);
   end;
end;

procedure TJvRegAuto.DestroyFile;
begin
 {$IFDEF COMPLIB_VCL}
  Reg.Free; Reg := nil;
 {$ENDIF COMPLIB_VCL}
 {$IFDEF LINUX}
  if Ini <> nil then
    Ini.UpdateFile;
 {$ENDIF LINUX}
  Ini.Free; Ini := nil;
  Str.Free; Str := nil;
  if FGlobalSettings then begin
    FIniFile := OldIniFile;
   {$IFDEF COMPLIB_VCL}
    if GlobalRegPath <> '' then FRegPath := GlobalRegPath;
    FRegPath := OldRegPath;
   {$ENDIF COMPLIB_VCL}
  end;
end;

procedure TJvRegAuto.EraseSection(const Section: string);
begin
  CreateFile;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
      Reg.EraseSection(ConcatSep(FSection, Section, '/'));
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      Ini.EraseSection(ConcatSep(FSection, Section, '/'));
   end;
  DestroyFile;
end;

procedure TJvRegAuto.DeleteKey(const Section, Ident: String);
begin
  CreateFile;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
      Reg.DeleteKey(ConcatSep(FSection, Section, '/'), Ident);
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      Ini.DeleteKey(ConcatSep(FSection, Section, '/'), Ident);
   end;
  DestroyFile;
end;

function TJvRegAuto.ReadString(const Section, Ident, Default: string): string;
begin
  Result := Default;
  CreateFile;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
      Result := Reg.ReadString(ConcatSep(FSection, Section, '/'), Ident, Default);
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      Result := Ini.ReadString(ConcatSep(FSection, Section, '/'), Ident, Default);
    raIniStrings:
      Result := Str.ReadString(ConcatSep(FSection, Section, '/'), Ident, Default);
   end;
  DestroyFile;
end;

procedure TJvRegAuto.WriteString(const Section, Ident, Value: string);
begin
  CreateFile;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
      Reg.WriteString(ConcatSep(FSection, Section, '/'), Ident, Value);
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      Ini.WriteString(ConcatSep(FSection, Section, '/'), Ident, Value);
    raIniStrings:
      Str.WriteString(ConcatSep(FSection, Section, '/'), Ident, Value);
   end;
  DestroyFile;
end;

function TJvRegAuto.ReadInteger(const Section, Ident: string;
  Default: Longint): Longint;
begin
  Result := Default;
  CreateFile;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
      Result := Reg.ReadInteger(ConcatSep(FSection, Section, '/'), Ident, Default);
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      Result := Ini.ReadInteger(ConcatSep(FSection, Section, '/'), Ident, Default);
    raIniStrings:
      Result := Str.ReadInteger(ConcatSep(FSection, Section, '/'), Ident, Default);
   end;
  DestroyFile;
end;

procedure TJvRegAuto.WriteInteger(const Section, Ident: string; Value: Longint);
begin
  CreateFile;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
      Reg.WriteInteger(ConcatSep(FSection, Section, '/'), Ident, Value);
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      Ini.WriteInteger(ConcatSep(FSection, Section, '/'), Ident, Value);
    raIniStrings:
      Str.WriteInteger(ConcatSep(FSection, Section, '/'), Ident, Value);
   end;
  DestroyFile;
end;

function TJvRegAuto.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  Result := Default;
  CreateFile;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
      Result := Reg.ReadBool(ConcatSep(FSection, Section, '/'), Ident, Default);
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      Result := Ini.ReadBool(ConcatSep(FSection, Section, '/'), Ident, Default);
    raIniStrings:
      Result := Str.ReadBool(ConcatSep(FSection, Section, '/'), Ident, Default);
   end;
  DestroyFile;
end;

procedure TJvRegAuto.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  CreateFile;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
      Reg.WriteBool(ConcatSep(FSection, Section, '/'), Ident, Value);
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      Ini.WriteBool(ConcatSep(FSection, Section, '/'), Ident, Value);
    raIniStrings:
      Str.WriteBool(ConcatSep(FSection, Section, '/'), Ident, Value);
   end;
  DestroyFile;
end;

function TJvRegAuto.ReadFloat(const Section, Ident: string; Default: Double): Double;
begin
  Result := Default;
  CreateFile;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
      Result := StrToFloat(Reg.ReadString(ConcatSep(FSection, Section, '/'), Ident, FloatToStr(Default)));
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      Result := Ini.ReadFloat(ConcatSep(FSection, Section, '/'), Ident, Default);
    raIniStrings:
      Result := Str.ReadFloat(ConcatSep(FSection, Section, '/'), Ident, Default);
   end;
  DestroyFile;
end;

procedure TJvRegAuto.WriteFloat(const Section, Ident: string; Value: Double);
begin
  CreateFile;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
      Reg.WriteString(ConcatSep(FSection, Section, '/'), Ident, FloatToStr(Value));
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      Ini.WriteFloat(ConcatSep(FSection, Section, '/'), Ident, Value);
    raIniStrings:
      Str.WriteFloat(ConcatSep(FSection, Section, '/'), Ident, Value);
   end;
  DestroyFile;
end;

procedure TJvRegAuto.ReadStrings(const Section, Ident : string; Strings : TStrings);
var
  S : string;
begin
  S := Strings.Text;
  S := ReplaceString(S, #13#10, '|');
  S := ReadString(ConcatSep(FSection, Section, '/'), Ident, S);
  S := ReplaceString(S, '|', #13#10);
  Strings.Text := S;
end;

procedure TJvRegAuto.WriteStrings(const Section, Ident: string; Value: TStrings);
var
  S : string;
begin
  S := Value.Text;
  S := ReplaceString(S, #13#10, '|');
  WriteString(ConcatSep(FSection, Section, '/'), Ident, S);
end;

procedure TJvRegAuto.ReadSection(const Section : string; Ss : TStrings);
begin
  CreateFile;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
      Reg.ReadSection(ConcatSep(FSection, Section, '/'), Ss);
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      Ini.ReadSection(ConcatSep(FSection, Section, '/'), Ss);
    raIniStrings:
      Str.ReadSection(ConcatSep(FSection, Section, '/'), Ss);
   end;
  DestroyFile;
end;

procedure TJvRegAuto.ReadSectionValues(const Section: string; Ss: TStrings);
begin
  CreateFile;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
      Reg.ReadSectionValues(ConcatSep(FSection, Section, '/'), Ss);
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      Ini.ReadSectionValues(ConcatSep(FSection, Section, '/'), Ss);
    raIniStrings:
      Str.ReadSectionValues(ConcatSep(FSection, Section, '/'), Ss);
   end;
  DestroyFile;
end;

procedure TJvRegAuto.ReadWholeSection(const Section : string; Ss : TStrings);
begin
  CreateFile;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
     { ReadWholeSection not supported for registry }
      Reg.ReadSection(ConcatSep(FSection, Section, '/'), Ss);
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      Ini.ReadWholeSection(ConcatSep(FSection, Section, '/'), Ss);
    raIniStrings:
      Str.ReadWholeSection(ConcatSep(FSection, Section, '/'), Ss);
   end;
  DestroyFile;
end;

procedure TJvRegAuto.ReadSections(Ss : TStrings);
begin
  CreateFile;
  case FStorage of
   {$IFDEF COMPLIB_VCL}
    raRegistry:
      Reg.ReadSections(Ss);
   {$ENDIF COMPLIB_VCL}
    raIniFile:
      Ini.ReadSections(Ss);
    raIniStrings:
      Str.ReadSections(Ss);
   end;
  DestroyFile;
end;

procedure TJvRegAuto.AddNotify(ANotify : TRegAutoEvent);
var
  Notify : ^TRegAutoEvent;
begin
  New(Notify);
  Notify^ := ANotify;
  FNotifiers.Add(Notify);
end;

procedure TJvRegAuto.RemoveNotify(ANotify : TRegAutoEvent);
var
  i : Integer;
  Notify : ^TRegAutoEvent;
begin
  for i := 0 to FNotifiers.Count - 1 do
  begin
    Notify := FNotifiers[i];
    if (TMethod(Notify^).Code = TMethod(ANotify).Code) and
      (TMethod(Notify^).Data = TMethod(ANotify).Data) then
    begin
      Dispose(Notify);
      FNotifiers.Delete(i);
      break;
    end;
  end;
end;

// support for old UseReg, UseIni, UseStr properties
procedure TJvRegAuto.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('UseReg', ReadUseRegProperty, nil, False);
  Filer.DefineProperty('UseIni', ReadUseIniProperty, nil, False);
  Filer.DefineProperty('UseStr', ReadUseStrProperty, nil, False);
end;

procedure TJvRegAuto.ReadUseRegProperty(Reader: TReader);
begin
  // ignore false values
  if Reader.ReadBoolean then
    UseReg := True;
end;

procedure TJvRegAuto.ReadUseIniProperty(Reader: TReader);
begin
  // ignore false values
  if Reader.ReadBoolean then
    UseIni := True;
end;

procedure TJvRegAuto.ReadUseStrProperty(Reader: TReader);
begin
  // ignore false values
  if Reader.ReadBoolean then
    UseStr := True; 
end;

{********************* TJvMyIniFile **********************}
procedure TJvMyIniFile.ReadWholeSection(const Section : string; Ss : TStrings);
var
  TmpSS: TStringList;
  TmpIniSS: TJvIniStrings;
begin
  TmpSS := TStringList.Create;
  try
    TmpSS.LoadFromFile(FileName);
    TmpIniSS := TJvIniStrings.Create(TmpSS);
    try
      TmpIniSS.ReadWholeSection(Section, SS);
    finally
      TmpIniSS.Free;
    end;
  finally
    TmpSS.Free;
  end;
end;

{$IFNDEF COMPILER35_Up}
function TJvMyIniFile.ReadFloat(const Section, Ident: string; Default: Double): Double;
begin
  Result := StrToFloat(ReadString(Section, Ident, FloatToStr(Default)));
end;

procedure TJvMyIniFile.WriteFloat(const Section, Ident: string; Value: Double);
begin
  WriteString(Section, Ident, FloatToStr(Value));
end;
{$ENDIF COMPILER35_Up}

{##################### TJvMyIniFile #####################}

{********************* TJvIniStrings **********************}
constructor TJvIniStrings.Create(AStrings : TStrings);
begin
  inherited Create;
  FStrings := AStrings;
end;

function TJvIniStrings.ReadString(const Section, Ident, Default: string): string;
var
  i : integer;
  S : string;
  P : integer;
begin
  Result := Default;
  i := FStrings.IndexOf('['+Section+']');
  if i = -1 then exit;
  inc(i);
  while i < FStrings.Count do begin
    S := FStrings[i];
    inc(i);
    if Length(S) = 0 then continue;
    if S[1] = '[' then exit;
    if ANSIStrLIComp(PChar(Ident), PChar(S), Length(Ident)) = 0 then begin
      P := Pos('=', S);
      if P <> 0 then
        Result := Copy(S, P+1, Length(S));
      exit;
    end;
  end;
end;

procedure TJvIniStrings.WriteString(const Section, Ident, Value: string);
var
  F: integer;
  S: string;
begin
  FStrings.BeginUpdate;
  F := FStrings.IndexOf('[' + Section + ']');
  if F > -1 then
  begin
    Inc(F);
    while F < FStrings.Count do
    begin
      S := Trim(FStrings[F]);
      if ((Length(S) > 0) and (Trim(S[1]) = '[')) or (Trim(S) = '') then
      begin
        FStrings.Insert(F, Ident + '=' + Value);
        Break;
      end
      else
        if UpperCase(Copy(S, 1, Pos('=', S) - 1)) = UpperCase(Ident) then
        begin
          FStrings[F] := Ident + '=' + Value;
          Break;
        end;
      Inc(F);
    end;
    if F >= FStrings.Count then FStrings.Add(Ident + '=' + Value);
  end
  else
  begin
    FStrings.Add('');
    FStrings.Add('[' + Section + ']');
    FStrings.Add(Ident + '=' + Value);
  end;
  FStrings.EndUpdate;
end;

function TJvIniStrings.ReadInteger(const Section, Ident: string;
  Default: Longint): Longint;
begin
  try
    Result := StrToInt(ReadString(Section, Ident, IntToStr(Default)));
  except
    Result := Default;
  end;
end;

procedure TJvIniStrings.WriteInteger(const Section, Ident: string; Value: Longint);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

function TJvIniStrings.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
var
  S : string;
begin
  S := Trim(ReadString(Section, Ident, IntToStr(integer(Default))));
  Result := (S = '1') or (ANSICompareText(S, 'on') = 0)  or (ANSICompareText(S, 'yes') = 0);
end;

procedure TJvIniStrings.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  WriteString(Section, Ident, IntToStr(integer(Value)));
end;

function TJvIniStrings.ReadFloat(const Section, Ident: string; Default: Double): Double;
begin
  try
    Result := StrToFloat(ReadString(Section, Ident, FloatToStr(Default)));
  except
    Result := Default;
  end;
end;

procedure TJvIniStrings.WriteFloat(const Section, Ident: string; Value: Double);
begin
  WriteString(Section, Ident, FloatToStr(Value));
end;

function TJvIniStrings.ReadSection(const Section : string; Ss : TStrings) : boolean;
var
  F : integer;
  S : string;

  procedure ReadSection1;
  begin
		inc(F);
		while F < FStrings.Count do begin
			S := FStrings[F];
			if (Length(S) > 0) and (Trim(S[1])= '[') then break;
      if Trim(S) <> '' then
  			Ss.Add(S);
			inc(F);
		end;
  end;    { ReadSection1 }

begin
  Ss.BeginUpdate;
  try
    Ss.Clear;
    F := FStrings.IndexOf('['+Section+']');
    Result := F > -1;
    if Result then
    begin
      ReadSection1;
      while F < FStrings.Count do
      begin
        S := Trim(FStrings[F]);
        if S = '['+Section+']' then
          ReadSection1
        else
          inc(F);
      end;    { while }
    end;
	finally
		Ss.EndUpdate;
	end;
end;

procedure TJvIniStrings.ReadSections(Ss : TStrings);
var
  i : integer;
  S : string;
begin
  Ss.Clear;
  for i := 0 to FStrings.Count - 1 do
  begin
    S := Trim(FStrings[i]);
    if (Length(S) > 0) and (S[1]= '[') and (S[Length(S)]= ']') then
      Ss.Add(Copy(S, 2, Length(S) - 2));
  end;
end;

procedure TJvIniStrings.ReadSectionValues(const Section: string; Ss: TStrings);
var
  F: integer;
  S: string;

  procedure ReadSection1;
  begin
    inc(F);
    while F < FStrings.Count do
    begin
      S := FStrings[F];
      if (Length(S) > 0) and (Trim(S[1]) = '[') then break;
      if Trim(S) <> '' then
        Ss.Add(S);
      inc(F);
    end;
  end; { ReadSection1 }

begin
  Ss.BeginUpdate;
  try
    Ss.Clear;
    F := FStrings.IndexOf('[' + Section + ']');
    if F > -1 then
    begin
      ReadSection1;
      while F < FStrings.Count do
      begin
        S := Trim(FStrings[F]);
        if S = '[' + Section + ']' then
          ReadSection1
        else
          inc(F);
      end; { while }
    end;
  finally
    Ss.EndUpdate;
  end;
end;

procedure TJvIniStrings.ReadWholeSection(const Section : string; Ss : TStrings);
var
  F : integer;
  S : string;
begin
  with FStrings do
  begin
  	F := IndexOf('['+Section+']');
  //	Result := F > -1;
  	if F > -1 then begin
  		Ss.BeginUpdate;
  		try
  			Ss.Clear;
  			inc(F);
  			while F < Count do begin
  				S := Strings[F];
  				if (Length(S) > 0) and (Trim(S[1])= '[') then break;
  				Ss.Add(S);
  				inc(F);
  			end;
  		finally
  			Ss.EndUpdate;
  		end;
  	end;
  end;  
end;

end.

