{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSAL.PAS, released on 2002-06-15.

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
unit JvSAL;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvSALHashList;

const

  stacklimit = 256;
  // message are processed every 250 milliseconds
  // use the stop procedure to stop a locked script
  timeout = 250;

type

  TonGetUnit = procedure(sender: TObject; aUnit: string; var aValue: string; var handled: boolean) of object;
  TJvAtom = class(TObject)
  private
    FValue: variant;
    Factor: TJvSALProc;
    procedure SetActor(const Value: TJvSALProc);
    procedure SetValue(const aValue: variant);
  public
    property Value: variant read FValue write SetValue;
    property Actor: TJvSALProc read Factor write Setactor;
  end;

  TJvSALProcAtom = class(TObject)
  private
    Fparser: TJvSALProc;
    Factor: TJvSALProc;
    procedure Setactor(const Value: TJvSALProc);
    procedure Setparser(const Value: TJvSALProc);
  public
    property actor: TJvSALProc read Factor write Setactor;
    property parser: TJvSALProc read Fparser write Setparser;
  end;
  TJvAtoms = class(TStringList)
  public
    procedure ClearAll;
    destructor Destroy; override;
  end;

  TJvSAL = class(TComponent)
  private
    FStop: boolean;
    FCaption: string;
    sp: integer;
    rsp: integer;
    bsp: integer;
    stack: array[0..stacklimit] of variant;
    bstack: array[0..stacklimit] of boolean;
    rstack: array[0..stacklimit] of integer;
    //    procs: TJvAtoms;
    procs: TJvSALHashList;
    Fscript: string;
    Funits: TStringList;
    ticks: cardinal;
    FonGetUnit: TonGetUnit;
    FVariableName: string;
    FVariable: TJvAtom;
    FtheSelect: variant;
    FuseDirective: string;
    FBeginOfComment: string;
    FEndOfComment: string;
    FstringDelim: string;
    Fpc: integer;
    Fatoms: TJvAtoms;
    Fpcproc: integer;
    Ftoken: string;
    procedure Setscript(const Value: string);
    procedure SetonGetUnit(const Value: TonGetUnit);
    procedure SetVariable(const Value: TJvAtom);
    procedure SetVariableName(const Value: string);
    procedure SettheSelect(const Value: variant);
    procedure SetuseDirective(const Value: string);
    procedure SetBeginOfComment(const Value: string);
    procedure SetEndOfComment(const Value: string);
    procedure SetstringDelim(const Value: string);
    procedure Setpc(const Value: integer);
    procedure Settoken(const Value: string);
    procedure SetCaption(const Value: string);
    { Private declarations }
  protected
    { Protected declarations }
    procedure parsescript;
    // return stack methods
    // SAL language
    procedure xbosub;
    procedure xeosub;
    procedure xValue;
    procedure xDefVariable;
    procedure xVariable;
    procedure xProc;
    procedure xnoParser;
  public
    { Public declarations }
    constructor create(AOwner: Tcomponent); override;
    destructor destroy; override;
    procedure ClearProcedures;
    procedure AddProcedure(aName: string; aProcedure, aParser: TJvSALProc);
    function apo(op: string; aProc: TJvSALProc): integer;
    procedure Push(aValue: variant);
    function Pop: variant;
    procedure rPush(aValue: integer);
    function rPop: integer;
    procedure boolPush(aValue: boolean);
    function boolPop: boolean;
    procedure LoadFromFile(fn: string);
    procedure execute;
    procedure stop;
    property pc: integer read Fpc write Setpc;
    property atoms: TJvAtoms read Fatoms;
    property pcproc: integer read Fpcproc;
    property token: string read Ftoken write Settoken;
    property script: string read Fscript write Setscript;
    property Caption: string read FCaption write SetCaption;
    property Variable: TJvAtom read FVariable write SetVariable;
    property VariableName: string read FVariableName write SetVariableName;
    property theSelect: variant read FtheSelect write SettheSelect;
    property useDirective: string read FuseDirective write SetuseDirective;
    property BeginOfComment: string read FBeginOfComment write SetBeginOfComment;
    property EndOfComment: string read FEndOfComment write SetEndOfComment;
    property stringDelim: string read FstringDelim write SetstringDelim;
  published
    { Published declarations }
    property onGetUnit: TonGetUnit read FonGetUnit write SetonGetUnit;
  end;

implementation

const
  cr = chr(13) + chr(10);
  tab = chr(9);

procedure SaveString(aFile, aText: string);
begin
  with TFileStream.Create(aFile, fmCreate) do
  try
    writeBuffer(aText[1], length(aText));
  finally free;
  end;
end;

function LoadString(aFile: string): string;
var
  s: string;
begin
  with TFileStream.Create(aFile, fmOpenRead) do
  try
    SetLength(s, Size);
    ReadBuffer(s[1], Size);
  finally free;
  end;
  result := s;
end;

{ TJvAtom }

procedure TJvAtom.Setactor(const Value: TJvSALProc);
begin
  Factor := Value;
end;

procedure TJvAtom.SetValue(const aValue: variant);
begin
  FValue := aValue;
end;

{ TJvAtoms }

procedure TJvAtoms.ClearAll;
var
  i: integer;
begin
  if count = 0 then exit;
  for i := 0 to count - 1 do
    //    TJvAtom(objects[i]).free;
    objects[i].free;
  Clear;
end;

destructor TJvAtoms.Destroy;
begin
  ClearAll;
  inherited;
end;

{ TJvSAL }

function TJvSAL.boolPop: boolean;
begin
  dec(bsp);
  if bsp < 0 then
    raise exception.create('boolean stack underflow');
  result := bstack[bsp];
end;

procedure TJvSAL.boolPush(aValue: boolean);
begin
  bstack[bsp] := aValue;
  inc(bsp);
  if bsp > stacklimit then
    raise exception.create('boolean stack overflow');
end;

constructor TJvSAL.create(AOwner: TComponent);
begin
  inherited;
  FAtoms := TJvAtoms.Create;
  Procs := TJvSALHashList.Create(ITinyHash, HashSecondaryOne, sameText);
  FUnits := TStringlist.create;
  FCaption := 'SAL';
  FuseDirective := 'use::';
  FBeginOfComment := '{';
  FEndOfComment := '}';
  fStringDelim := '"';
end;

destructor TJvSAL.destroy;
begin
  Atoms.free;
  Procs.Free;
  Funits.free;
  inherited;
end;

procedure TJvSAL.execute;
var
  a: TJvAtom;
  c: integer;
begin
  pc := 0;
  sp := 0;
  rsp := 0;
  bsp := 0;
  c := atoms.Count;
  Fstop := false;
  ticks := gettickcount;
  if c = 0 then exit;
  repeat
    a := TJvAtom(atoms.objects[pc]);
    Fpcproc := pc;
    inc(fpc);
    a.actor;
    if (gettickcount - ticks) > timeout then
    begin
      ticks := gettickcount;
      application.ProcessMessages;
    end;
    if FStop then
      raise exception.create('program stopped');
  until pc >= c;
end;

procedure TJvSAL.parsescript;
var
  s: string;
  //  iprocs:integer;
  haveproc: boolean;
  aActor: TJvSALProc;
  aParser: TJvSALProc;
  i, p, p2: integer;
  fv: double;
  a: TJvAtom;
  fn, theunit: string;
  handled: boolean;

  function charfrom(from: integer; achar: char; aText: string): integer;
  var
    cc: integer;
  begin
    result := 0;
    cc := length(aText);
    repeat
      if aText[from] = achar then
      begin
        result := from;
        exit;
      end;
      inc(from);
    until from > cc;
  end;
begin
  pc := 1;
  s := FScript;
  FUnits.Clear;
  // process any includes
  repeat
    p := pos(fuseDirective, s); // default use::
    if p > 0 then
    begin
      p2 := charfrom(p, ' ', s);
      if p2 = 0 then
        raise exception.create('unterminated include directive near ' + copy(s, p, 50));
      fn := trim(copy(s, p + length(fuseDirective), p2 - p - length(fuseDirective)));
      if not assigned(onGetUnit) then
        raise exception.create('ongetUnit event handler is not assigned');
      handled := false;
      fn := lowercase(fn);
      if FUnits.IndexOf(fn) = -1 then
      begin
        onGetUnit(self, fn, theunit, handled);
        if not handled then
          raise exception.create('could not include unit ' + fn);
        theunit := stringreplace(theunit, cr, ' ', [rfreplaceall]);
        delete(s, p, p2 - p);
        insert(theunit, s, p);
        FUnits.Append(fn);
      end;
    end;
  until p = 0;
  while s <> '' do
  begin
    if pos(fBeginOfComment, s) = 1 then
    begin // default= {
      p := pos(fEndOfComment, s); // default= }
      if p = 0 then
        raise exception.create('unterminated comment near ' + s);
      delete(s, 1, p + length(fEndOfComment) - 1);
      s := trim(s);
    end
    else if pos(fStringDelim, s) = 1 then
    begin // default = "
      delete(s, 1, length(fStringDelim));
      p := pos(fStringDelim, s);
      if p = 0 then
        raise exception.create('unterminated string near ' + s);
      token := copy(s, 1, p - 1);
      delete(s, 1, p + length(fStringDelim) - 1);
      s := trim(s);
      a := TJvAtom.Create;
      a.Value := token;
      a.actor := xValue;
      atoms.AddObject('literal', a);
    end
    else
    begin
      p := pos(' ', s);
      if p = 0 then
      begin
        token := s;
        s := '';
      end
      else
      begin
        token := copy(s, 1, p - 1);
        delete(s, 1, p);
        s := trim(s);
      end;
      // take care of aliasis
      if token = '.' then token := '+=';
      // check for user procs
      haveproc := procs.Hash(token, aActor, aParser);
      try // float
        fv := strtofloat(token);
        a := TJvAtom.Create;
        a.Value := fv;
        a.actor := xValue;
        atoms.AddObject('literal', a);
      except //
        if pos('proc-', token) = 1 then
        begin // begin of procedure
          if pos('end-proc', s) = 0 then
            raise exception.create('unterminated procedure near' + s);
          apo(token, xbosub);
        end
        else if token = 'end-proc' then
          apo(token, xeosub)
        else if copy(token, length(token) - 1, 2) = '()' then
          apo(token, xproc) // proc call
        else if pos('var-', token) = 1 then
        begin // define variable
          if atoms.IndexOf(token) <> -1 then
            raise exception.Create('variable ' + token + ' allready defined;' + s);
          a := TJvAtom.create;
          a.actor := xDefVariable;
          atoms.AddObject(token, a);
        end
        else if token[1] = '$' then
        begin // variable value
          // find address
          i := atoms.IndexOf('var-' + copy(token, 2, maxint));
          if i = -1 then
            raise exception.Create('variable ' + token + ' is not yet defined;' + s);
          a := TJvAtom.create;
          a.Value := i;
          a.actor := xVariable;
          atoms.AddObject(token, a);
        end
        else if haveproc then
        begin
          if @aparser <> nil then
            aparser
          else
            apo(Token, aActor);
        end
        else
          raise exception.create('procedure ' + token + ' near ' + s);
      end
    end
  end;
  // now resolve procs()
  if atoms.count = 0 then exit;
  for i := 0 to atoms.count - 1 do
  begin
    s := atoms[i];
    if copy(s, length(s) - 1, 2) = '()' then
    begin
      s := 'proc-' + copy(s, 1, length(s) - 2);
      p := atoms.indexof(s);
      if p = -1 then
        raise exception.create('undefined procedure ' + s);
      TJvAtom(atoms.objects[i]).value := p;
    end;
  end;
end;

function TJvSAL.Pop: variant;
begin
  dec(sp);
  if sp < 0 then
    raise exception.create('stack underflow');
  result := stack[sp];
end;

procedure TJvSAL.Push(aValue: variant);
begin
  stack[sp] := aValue;
  inc(sp);
  if sp > stacklimit then
    raise exception.create('stack overflow');
end;

procedure TJvSAL.Setscript(const Value: string);
begin
  Fscript := trim(stringreplace(Value, cr, ' ', [rfreplaceall]));
  Atoms.ClearAll;
  parsescript;
end;

procedure TJvSAL.xDefVariable;
var
  a: TJvAtom;
begin
  a := TJvAtom(atoms.objects[pcproc]);
  FvariableName := atoms[pcproc];
  Fvariablename := '$' + copy(Fvariablename, 5, maxint);
  Fvariable := a;
end;

procedure TJvSAL.xValue;
begin
  push(TJvAtom(atoms.objects[pcproc]).value);
end;

procedure TJvSAL.xVariable;
var
  index: integer;
  a: TJvAtom;
begin
  a := TJvAtom(atoms.objects[pcproc]);
  variableName := atoms[pcproc];
  index := a.Value;
  variable := TJvAtom(atoms.objects[index]);
end;

procedure TJvSAL.stop;
begin
  FStop := true;
end;

procedure TJvSAL.LoadFromFile(fn: string);
begin
  script := loadstring(fn);
end;

procedure TJvSAL.ClearProcedures;
begin
  //  procs.ClearAll;
  procs.Clear;
end;

procedure TJvSAL.AddProcedure(aName: string; aProcedure, aParser: TJvSALProc);
//var
//  a:TJvSALProcAtom;
begin
  //  a:=TJvSALProcAtom.Create;
  //  a.actor:=aProcedure;
  //  a.parser:=aParser;
  //  procs.AddObject(aName,a);
  procs.AddString(aName, aProcedure, aParser);
end;

function TJvSAL.rPop: integer;
begin
  dec(rsp);
  if rsp < 0 then
    raise exception.create('return stack underflow');
  result := rstack[rsp];
end;

procedure TJvSAL.rPush(aValue: integer);
begin
  rstack[rsp] := aValue;
  inc(rsp);
  if rsp > stacklimit then
    raise exception.create('return stack overflow');
end;

// end of subroutine, marked with end-proc

procedure TJvSAL.xeosub;
begin
  pc := rpop;
end;

// begin of subroutine, marked with [
// loop to ]

procedure TJvSAL.xbosub;
var
  op: string;
  c: integer;
begin
  c := atoms.count;
  repeat
    op := atoms[pc];
    inc(fpc);
    if op = 'end-proc' then exit;
  until pc >= c;
  raise exception.Create('could not find end of procedure');
end;

procedure TJvSAL.SetonGetUnit(const Value: TonGetUnit);
begin
  FonGetUnit := Value;
end;

// function call

procedure TJvSAL.xProc;
var
  index: integer;
begin
  index := TJvAtom(atoms.objects[pcproc]).value;
  rpush(pc);
  pc := index + 1;
end;

procedure TJvSAL.SetVariable(const Value: TJvAtom);
begin
  FVariable := Value;
end;

procedure TJvSAL.SetVariableName(const Value: string);
begin
  FVariableName := Value;
end;

procedure TJvSAL.SettheSelect(const Value: variant);
begin
  FtheSelect := Value;
end;

procedure TJvSAL.SetuseDirective(const Value: string);
begin
  FuseDirective := Value;
end;

procedure TJvSAL.SetBeginOfComment(const Value: string);
begin
  FBeginOfComment := Value;
end;

procedure TJvSAL.SetEndOfComment(const Value: string);
begin
  FEndOfComment := Value;
end;

procedure TJvSAL.SetstringDelim(const Value: string);
begin
  FstringDelim := Value;
end;

procedure TJvSAL.Setpc(const Value: integer);
begin
  Fpc := Value;
end;

function TJvSAL.apo(op: string; aProc: TJvSALProc): integer;
var
  a: TJvAtom;
begin
  a := TJvAtom.create;
  a.actor := aProc;
  result := atoms.AddObject(op, a);
end;

procedure TJvSAL.Settoken(const Value: string);
begin
  Ftoken := Value;
end;

procedure TJvSAL.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TJvSAL.xnoParser;
begin
  // do nothing
end;

{ TJvSALProcAtom }

procedure TJvSALProcAtom.Setactor(const Value: TJvSALProc);
begin
  Factor := Value;
end;

procedure TJvSALProcAtom.Setparser(const Value: TJvSALProc);
begin
  Fparser := Value;
end;

end.
