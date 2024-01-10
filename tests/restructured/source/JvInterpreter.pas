{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : JvInterpreterProgram and more..
description : JVCL Interpreter version 2

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

{ history (JVCL Library versions):
  1.10:
   - first release;
  1.12:
   - method HandleException removed as bugged;
   - method UpdateExceptionPos now fill error message
     with error Unit name and Line pos;
   - fixed bug in TJvInterpreterUnit.Assignment method;
   - new public property BaseErrLine used in UpdateExceptionPos;
  1.17.7:
   - local "const" statement for functions;
   - global variables and constants - scope on all units - not normal !;
   - OnGetValue and OnSetValue now called before call to Adapter;
   - fixed bug with "Break" statement inside "for" loop;
  1.17.10:
   - fixed(?) bug with "begin/end" statement in "else" part of "if" statement;
   - fixed few bugs in ole automation support;
  1.21.2 (RALib 1.21 Update 2):
   - fixed bug with multiple external functions defintions
     (greetings to Peter Fischer-Haaser)
   - fixed AV-bug in TJvInterpreterFunction.InFunction1 if errors in source occurred
     (greetings to Andre N Belokon)
  1.21.4 (RALib 1.21 Update 4):
   - fixed bugs in "if" and "while" with "begin" statements;
   - "div" and "mod" now working;
  1.21.6 (RALib 1.21 Update 6):
   - fixed bug with incorrect error line and unit name if erorr
     occurred in used unit
     (greetings to Dmitry Mokrushin)
   - add parameters check (not fully functional - only count checked)
     in source fucntion calls;
  1.31.2 (RALib 1.31 Update 2):
   - fixed bug: sometimes compare-operators ('=', '>', ...)
     in expressions do not working.
  1.31.4 (RALib 1.31 Update 4):
   - fixed bug: plus and minus operators after symbol ']' not working.
  1.31.5 (RALib 1.31 Update 5):
   - function Statement1 is changed; this remove many bugs and add new ones.
   - fixed many bug in exception handling statements and in nested
     "begin/end" statements;
   - fixed error with source function with TObject (and descendants)
     returning values;
  1.41.1:
   - another fix for bug with incorrect error line and unit name
     if erorr occurred in used unit;
   - fixed bug with "break" statement;
   - "exit" statement;
   - "repeat" loop;
  1.50:
   - behavior of "UseGlobalAdapter" property was changed; in previous versions
     each TJvInterpreterExpression component creates its own copy of GlobalAdapter and
     then manage it own copy, but now TJvInterpreterExpression manages two adapters:
     own and global, so GlobalJvInterpreterAdapter now is used by all TJvInterpreterExpressions;
     performance of "Compile" function increased (there is no necessity
     more to Assign adapters) (20 msec on my machine with JvInterpreter_all unit)
     and memory requirement decreased;
   - sorting in TJvInterpreterAdapter dramatically increase its performance speed;
   - fixed bug in "except/on" statement;
  1.51:
   - arrays as local and global variables. supports simple types (integer,
     double, string, tdatetime, object).
     Added by Andrej Olejnik (olej@asset.sk);
   - type conversion with integer, string, TObject,... keywords;
  1.51.2:
   - array support was rewritten;
     enhanced indexes support: default indexed properties,
     access to chars in strings. Many changes are made to make this possible:
     new methods: GetElement, SetElement;
   - record support is simplified;
   - new property TJvInterpreterExpression.Error provide extended error information
     about non-interpreter errors.
   - "case" statement; not fully implemented - only one expression for one block.
  1.52:
   - TJvInterpreterExpression.JvInterpreterAdapter property renamed to Adapter;
   - new public property TJvInterpreterExpression.SharedAdapter, setting to
     GlobalJvInterpreterAdapter by default. This allows to create set of global adapters,
     shared between TJvInterpreterExpression components;
   - property TJvInterpreterExpression.GlobalAdapter removed; setting SharedAdapter
     to nil has same effect as GlobalAdapter := False;
   - fixed memory bug in event handling;
   - new: unit name in uses list can be placed in quotes and contains any symbols;
   - fixed bug: selector in case-statement not working with variables (only constants)
  1.53:
   - fixed bug: "Type mistmatch error" in expressions with OleAutomation objects;
   - fixed bug: error while assign function's result to object's published property; 
   - call to external functions (placed in dll) in previous versions always
     return integer, now it can return boolean, if declared so;
  1.54:
   - new: in call to external function var-parameters are supported for
     integer type;
   - new: after call to external function (placed in dll) last win32 error
     is restored correctly; in previous versions it was overriden by call to
     FreeLibrary;
   - fixed bug: memory leak: global variables and constants not allways be freed;
  1.60:
   - bug fixed in case-statement;
   - new: global variables and constants in different units now can have
     identical names;
   - new: constants, variables and functions can have prefix with unit name
     and point to determine appropriate unit;
   - new: class declaration for forms (needed for TJvInterpreterFm component);
   - bug fixed: record variables do not work;
  1.61:
   - bug fixed: variable types are not always kept the same when
     assigning values to them;
     thanks to Ritchie Annand (RitchieA@malibugroup.com);
   - bug fixed: exceptions, raised in dll calls produce AV.
     fix: exception of class Exception is raised.
   - new internal: LocalVars property in TJvInterpreterFunction (it is used in TJvInterpreterFm).
  2.00:
   - Delphi 6 compatibility;
   - Kylix 1 compatibility;
   - exception handling was rewriten in more portable way,
     ChangeTopException function is not used anymore;
   - fixed bug: intefrace section was not processed correct
     (Thanks to Ivan Ravin);



{.$DEFINE JvInterpreter_DEBUG}

{$IFDEF COMPLIB_VCL}
  {$DEFINE JvInterpreter_OLEAUTO}
{$ENDIF COMPLIB_VCL}

unit JvInterpreter;

interface

uses SysUtils, Classes, JvInterpreterParser
{$IFDEF COMPILER6_UP}
  , Variants
{$ENDIF COMPILER6_UP}
{$IFDEF MSWINDOWS}
  , Windows
{$ENDIF MSWINDOWS}
  ;

const

 { max arguments to functions - small values increase performance }
  MaxArgs = 32;

 { max fields allowed in records }
  MaxRecFields = 32;

type

 { argument definition }
  TValueArray = array[0..MaxArgs] of Variant;
  TTypeArray = array[0..MaxArgs] of Word;
  TNameArray = array[0..MaxArgs] of string;
  PValueArray = ^TValueArray;
  PTypeArray = ^TTypeArray;
  PNameArray = ^TNameArray;

  TArgs = class;

  TJvInterpreterGetValue = procedure(Sender: TObject; Identifer: string; var Value: Variant;
    Args: TArgs; var Done: Boolean) of object;
  TJvInterpreterSetValue = procedure(Sender: TObject; Identifer: string;
    const Value: Variant; Args: TArgs; var Done: Boolean) of object;
  TJvInterpreterGetUnitSource = procedure(UnitName: string; var Source: string;
    var Done: Boolean) of object;

  TJvInterpreterAdapterGetValue = procedure(var Value: Variant; Args: TArgs);
  TJvInterpreterAdapterSetValue = procedure(const Value: Variant; Args: TArgs);
  TJvInterpreterAdapterNewRecord = procedure(var Value: Pointer);
  TJvInterpreterAdapterDisposeRecord = procedure(const Value: Pointer);
  TJvInterpreterAdapterCopyRecord = procedure(var Dest: Pointer; const Source: Pointer);

  TOpenArray = array[0..MaxArgs] of TVarRec;

  TArgs = class
  private
    VarNames: TNameArray;
    HasVars: Boolean;
  public
    Identifer: string;
    Count: Integer;
    Types: TTypeArray;
    Values: TValueArray;
    Names: TNameArray;
    HasResult: Boolean; { = False, if result not needed - used by calls
                          to ole automation servers }
    Assignment: Boolean; { internal }
    Obj: TObject;
    ObjTyp: Word; { varObject, varClass, varUnknown }

    Indexed: Boolean; // if True then Args contain Indexes to Identifer
    ReturnIndexed: Boolean; // established by GetValue function, indicating
                            // what Args used as indexed (matters only if Indexed = True)
  public
    destructor Destroy; override;
    procedure Clear;
    procedure OpenArray(const Index: Integer);
    procedure Delete(const Index: Integer);
  private
   { open array parameter support }
    { allocates memory only if necessary }
    OAV: ^TValueArray; { open array values }
  public
   { open array parameter support }
    OA: ^TOpenArray; { open array }
    OAS: Integer; { open array size }
  end;

 { function descriptor }
  TJvInterpreterFunDesc = class
  private
    FUnitName: string;
    FIdentifer: string;
    FClassIdentifer: string;  { class name, if function declared as
                                TClassIdentifer.Identifer}
    FParamCount: Integer;  { - 1..MaxArgs }
    FParamTypes: TTypeArray;
    FParamNames: TNameArray;
    FResTyp: Word;
    FPosBeg: Integer; { position in source }
    FPosEnd: Integer;
    function GetParamName(Index: Integer): string;
    function GetParamType(Index: Integer): Word;
  public
    property UnitName: string read FUnitName;
    property Identifer: string read FIdentifer;
    property ClassIdentifer: string read FClassIdentifer;
    property ParamCount: Integer read FParamCount;
    property ParamTypes[Index: Integer]: Word read GetParamType;
    property ParamNames[Index: Integer]: string read GetParamName;
    property ResTyp: Word read FResTyp;
    property PosBeg: Integer read FPosBeg;
    property PosEnd: Integer read FPosEnd;
  end;

  TSimpleEvent = procedure of object;
  TJvInterpreterExpression = class;
  EJvInterpreterError = class;

  TJvInterpreterEvent = class(TObject)
  private
    Owner: TJvInterpreterExpression;
    Instance: TObject;
    UnitName: string;
    FunName: string;
    FArgs: TArgs;
    function GetArgs: TArgs;
  protected
    constructor Create(AOwner: TJvInterpreterExpression; AInstance: TObject;
      AUnitName, AFunName: string); virtual;
    function CallFunction(Args: TArgs; Params: array of Variant): Variant;
    property Args: TArgs read GetArgs;
  public
    destructor Destroy; override;
  end;

  TJvInterpreterEventClass = class of TJvInterpreterEvent;

 { variable holder }
  TJvInterpreterVar = class
  public
    UnitName: string;
    Identifer: string;
    Typ: string;
    VTyp: Word;
    Value: Variant;
  public
    destructor Destroy; override;
  end;

 { variables list }
  TJvInterpreterVarList = class(TList)
  public
    destructor Destroy; override;
    procedure Clear; {$IFDEF COMPILER35_Up} override; {$ENDIF}
    procedure AddVar(UnitName, Identifer, Typ: string; VTyp: Word;
      const Value: Variant);
    function FindVar(const UnitName, Identifer: string): TJvInterpreterVar;
    procedure DeleteVar(const UnitName, Identifer: string);
    function GetValue(Identifer: string; var Value: Variant; Args: TArgs)
      : Boolean;
    function SetValue(Identifer: string; const Value: Variant; Args: TArgs)
      : Boolean;
  end;
 { notes about TJvInterpreterVarList implementation:
   - list must allows to contain more than one Var with same names;
   - FindVar must return last added Var with given name;
   - DeleteVar must delete last added Var with given name; }

  TJvInterpreterIdentifer = class
  public
    UnitName: string;
    Identifer: string;
    Data: Pointer;  // provided by user when call to adapter's addxxx methods
  end;

  TJvInterpreterIdentiferList = class(TList)
  private
    FDuplicates: TDuplicates;
  public
    function IndexOf(const UnitName, Identifer: string): TJvInterpreterIdentifer;
    function Find(const Identifer: string; var Index: Integer): Boolean;
    procedure Sort;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  TJvInterpreterRecField = record
    Identifer: string;
    Offset: Integer;
    Typ: Word;
  end;

  TCallConvention = set of (ccFastCall, ccStdCall, ccCDecl, ccDynamic,
    ccVirtual, ccClass);

 { TJvInterpreterAdapter - route JvInterpreter calls to Delphi functions }
  TJvInterpreterAdapter = class
  private
    FOwner: TJvInterpreterExpression;
    FSrcUnitList: TJvInterpreterIdentiferList; // JvInterpreter-units sources
    FExtUnitList: TJvInterpreterIdentiferList; // internal units; like "system" in delphi
    FGetList: TJvInterpreterIdentiferList;     // methods
    FSetList: TJvInterpreterIdentiferList;     // write properties
    FIGetList: TJvInterpreterIdentiferList;    // read indexed properties
    FISetList: TJvInterpreterIdentiferList;    // write indexed properties
    FIDGetList: TJvInterpreterIdentiferList;   // read default indexed properties
    FIDSetList: TJvInterpreterIdentiferList;   // write default indexed properties
    FDGetList: TJvInterpreterIdentiferList;    // direct get list
    FClassList: TJvInterpreterIdentiferList;   // delphi classes
    FConstList: TJvInterpreterIdentiferList;   // delphi consts
    FFunList: TJvInterpreterIdentiferList;     // functions, procedures
    FRecList: TJvInterpreterIdentiferList;     // records
    FRecGetList: TJvInterpreterIdentiferList;  // read record field
    FRecSetList: TJvInterpreterIdentiferList;  // write record field
    FOnGetList: TJvInterpreterIdentiferList;   // chain
    FOnSetList: TJvInterpreterIdentiferList;   // chain
    FSrcFunList: TJvInterpreterIdentiferList;  // functions, procedures in JvInterpreter-source
    FExtFunList: TJvInterpreterIdentiferList;
    FEventHandlerList: TJvInterpreterIdentiferList;
    FEventList: TJvInterpreterIdentiferList;
    FSrcVarList: TJvInterpreterVarList;         // variables, constants in JvInterpreter-source
    FSrcClassList: TJvInterpreterIdentiferList; // JvInterpreter-source classes

    FSorted: Boolean;
    procedure CheckArgs(var Args: TArgs; ParamCount: Integer;
      var ParamTypes: TTypeArray);
    function GetRec(RecordType: string): TObject;
   {$IFDEF JvInterpreter_OLEAUTO}
    function DispatchCall(Identifer: string; var Value: Variant;
      Args: TArgs; Get: Boolean): Boolean; stdcall;
   {$ENDIF JvInterpreter_OLEAUTO}
    function GetValueRTTI(Identifer: string; var Value: Variant;
      Args: TArgs): Boolean;
    function SetValueRTTI(Identifer: string; const Value: Variant;
      Args: TArgs): Boolean;
  protected
    procedure CheckAction(Expression: TJvInterpreterExpression; Args: TArgs;
      Data: Pointer); virtual;
    function GetValue(Expression: TJvInterpreterExpression; Identifer: string;
      var Value: Variant; Args: TArgs): Boolean; virtual;
    function SetValue(Expression: TJvInterpreterExpression; Identifer: string;
      const Value: Variant; Args: TArgs): Boolean; virtual;
    function GetElement(Expression: TJvInterpreterExpression; const Variable: Variant;
      var Value: Variant; var Args: TArgs): Boolean; virtual;
    function SetElement(Expression: TJvInterpreterExpression; var Variable: Variant;
      const Value: Variant; var Args: TArgs): Boolean; virtual;
    function SetRecord(var Value: Variant): Boolean; virtual;
    function NewRecord(const RecordType: string; var Value: Variant)
      : Boolean; virtual;
    function FindFunDesc(const UnitName: string; const Identifer: string)
      : TJvInterpreterFunDesc; virtual;
    procedure CurUnitChanged(NewUnitName: string; var Source: string); virtual;
    function UnitExists(const Identifer: string): Boolean; virtual;
    function IsEvent(Obj: TObject; const Identifer: string): Boolean; virtual;
    function NewEvent(const UnitName: string; const FunName, EventType: string;
      AOwner: TJvInterpreterExpression; AObject: TObject): TSimpleEvent; virtual;
    procedure ClearSource; dynamic;
    procedure ClearNonSource; dynamic;
    procedure Sort; dynamic;
  protected
    { for internal use }
    procedure AddSrcClass(JvInterpreterSrcClass: TJvInterpreterIdentifer); virtual;
    function GetSrcClass(Identifer: string): TJvInterpreterIdentifer; virtual;
  public
    constructor Create(AOwner: TJvInterpreterExpression);
    destructor Destroy; override;
    procedure Clear; dynamic;
    procedure Assign(Source: TJvInterpreterAdapter); dynamic;
    procedure AddSrcUnit(Identifer: string; Source: string; UsesList: string);
      dynamic;
    procedure AddSrcUnitEx(Identifer: string; Source: string; UsesList: string;
      Data: Pointer); dynamic;
    procedure AddExtUnit(Identifer: string); dynamic;
    procedure AddExtUnitEx(Identifer: string; Data: Pointer); dynamic;
    procedure AddClass(UnitName: string; ClassType: TClass; Identifer: string);
      dynamic;
    procedure AddClassEx(UnitName: string; ClassType: TClass; Identifer: string;
      Data: Pointer); dynamic;
    procedure AddGet(ClassType: TClass; Identifer: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word); dynamic;
    procedure AddGetEx(ClassType: TClass; Identifer: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word; Data: Pointer); dynamic;
    procedure AddSet(ClassType: TClass; Identifer: string;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word); dynamic;
    procedure AddSetEx(ClassType: TClass; Identifer: string;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word; Data: Pointer); dynamic;
    procedure AddIGet(ClassType: TClass; Identifer: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word); dynamic;
    procedure AddIGetEx(ClassType: TClass; Identifer: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word; Data: Pointer); dynamic;
    procedure AddISet(ClassType: TClass; Identifer: string;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word); dynamic;
    procedure AddISetEx(ClassType: TClass; Identifer: string;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word; Data: Pointer); dynamic;
    procedure AddIDGet(ClassType: TClass;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word); dynamic;
    procedure AddIDGetEx(ClassType: TClass;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word; Data: Pointer); dynamic;
    procedure AddIDSet(ClassType: TClass;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word); dynamic;
    procedure AddIDSetEx(ClassType: TClass;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word; Data: Pointer); dynamic;
    procedure AddFun(UnitName: string; Identifer: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word); dynamic;
    procedure AddFunEx(UnitName: string; Identifer: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word; Data: Pointer); dynamic;
   { function AddDGet under construction - don't use it }
    procedure AddDGet(ClassType: TClass; Identifer: string;
      GetFunc: Pointer; ParamCount: Integer; ParamTypes: array of Word;
      ResTyp: Word; CallConvention: TCallConvention); dynamic;
    procedure AddDGetEx(ClassType: TClass; Identifer: string;
      GetFunc: Pointer; ParamCount: Integer; ParamTypes: array of Word;
      ResTyp: Word; CallConvention: TCallConvention; Data: Pointer); dynamic;
    procedure AddRec(UnitName: string; Identifer: string; RecordSize: Integer;
      Fields: array of TJvInterpreterRecField; CreateFunc: TJvInterpreterAdapterNewRecord;
      DestroyFunc: TJvInterpreterAdapterDisposeRecord; CopyFunc: TJvInterpreterAdapterCopyRecord);
      dynamic;
    procedure AddRecEx(UnitName: string; Identifer: string; RecordSize: Integer;
      Fields: array of TJvInterpreterRecField; CreateFunc: TJvInterpreterAdapterNewRecord;
      DestroyFunc: TJvInterpreterAdapterDisposeRecord; CopyFunc: TJvInterpreterAdapterCopyRecord;
      Data: Pointer); dynamic;
    procedure AddRecGet(UnitName: string; RecordType: string; Identifer: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word); dynamic;
    procedure AddRecGetEx(UnitName: string; RecordType: string; Identifer: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word; Data: Pointer); dynamic;
    procedure AddRecSet(UnitName: string; RecordType: string; Identifer: string;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word); dynamic;
    procedure AddRecSetEx(UnitName: string; RecordType: string; Identifer: string;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word; Data: Pointer); dynamic;
    procedure AddConst(UnitName: string; Identifer: string; Value: Variant);
      dynamic;
    procedure AddConstEx(UnitName: string; Identifer: string; Value: Variant;
      Data: Pointer); dynamic;
    procedure AddExtFun(UnitName: string; Identifer: string; DllInstance: HINST;
      DllName: string; FunName: string; FunIndex: integer; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word); dynamic;
    procedure AddExtFunEx(UnitName: string; Identifer: string; DllInstance: HINST;
      DllName: string; FunName: string; FunIndex: integer; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word; Data: Pointer); dynamic;
    procedure AddSrcFun(UnitName: string; Identifer: string;
      PosBeg, PosEnd: integer; ParamCount: Integer; ParamTypes: array of Word;
      ParamNames: array of string; ResTyp: Word; Data: Pointer); dynamic;
    procedure AddSrcFunEx(UnitName: string; Identifer: string;
      PosBeg, PosEnd: integer; ParamCount: Integer; ParamTypes: array of Word;
      ParamNames: array of string; ResTyp: Word; Data: Pointer); dynamic;
    procedure AddHandler(UnitName: string; Identifer: string;
      EventClass: TJvInterpreterEventClass; Code: Pointer); dynamic;
    procedure AddHandlerEx(UnitName: string; Identifer: string;
      EventClass: TJvInterpreterEventClass; Code: Pointer; Data: Pointer); dynamic;
    procedure AddEvent(UnitName: string; ClassType: TClass;
      Identifer: string); dynamic;
    procedure AddEventEx(UnitName: string; ClassType: TClass;
      Identifer: string; Data: Pointer); dynamic;
    procedure AddSrcVar(UnitName: string; Identifer, Typ: string; VTyp: Word;
      const Value: Variant); dynamic;
    procedure AddOnGet(Method: TJvInterpreterGetValue); dynamic;
    procedure AddOnSet(Method: TJvInterpreterSetValue); dynamic;
  end;

  TStackPtr = - 1..99;

 { Expression evaluator }
  TJvInterpreterExpression = class(TComponent)
  private
    Parser: TJvInterpreterParser;
    FVResult: Variant;
    ExpStack: array[0..99] of Variant;
    ExpStackPtr: TStackPtr;
    Token1: Variant;
    FBacked: Boolean;
    TTyp1: TTokenTyp;
    TokenStr1: string;
    PrevTTyp: TTokenTyp;
    AllowAssignment: Boolean;
    FArgs: TArgs; { data }
    Args: TArgs; { pointer to current }
    FPStream: TStream; { parsed source }
    FParsed: Boolean;
    FAdapter: TJvInterpreterAdapter;
    FSharedAdapter: TJvInterpreterAdapter;
    FCompiled: Boolean;
    FBaseErrLine: Integer;
    FOnGetValue: TJvInterpreterGetValue;
    FOnSetValue: TJvInterpreterSetValue;
    FLastError: EJvInterpreterError;
    function GetSource: string;
    procedure SetSource(Value: string);
    procedure SetCurPos(Value: Integer);
    function GetCurPos: Integer;
    function GetTokenStr: string;
    procedure ReadArgs;
    procedure InternalGetValue(Obj: Pointer; ObjTyp: Word; var Result: Variant);
    function CallFunction(const FunName: string;
      Args: TArgs; Params: array of Variant): Variant; virtual; abstract;
    function CallFunctionEx(Instance: TObject; const UnitName: string;
      const FunName: string; Args: TArgs; Params: array of Variant)
      : Variant; virtual; abstract;
  protected
    procedure UpdateExceptionPos(E: Exception; const UnitName: string);
    procedure Init; dynamic;
    procedure ErrorExpected(Exp: string);
    procedure ErrorNotImplemented(Message: string);
    function PosBeg: Integer;
    function PosEnd: Integer;
    procedure Back;
    procedure SafeBack; {? please don't use ?}
    function CreateAdapter: TJvInterpreterAdapter; dynamic;

    procedure ParseToken;
    procedure ReadToken;
    procedure WriteToken;
    procedure Parse;

    function Expression1: Variant;
    function Expression2(const ExpType: Word): Variant;
    function SetExpression1: Variant;

    procedure NextToken;
    function GetValue(Identifer: string; var Value: Variant;
      var Args: TArgs): Boolean; virtual;
    function SetValue(Identifer: string; const Value: Variant;
      var Args: TArgs): Boolean; virtual;
    function GetElement(const Variable: Variant; var Value: Variant;
      var Args: TArgs): Boolean; virtual;
    function SetElement(var Variable: Variant; const Value: Variant;
      var Args: TArgs): Boolean; virtual;
    procedure SourceChanged; dynamic;
    procedure SetAdapter(Adapter: TJvInterpreterAdapter);
    property Token: Variant read Token1;
    property TTyp: TTokenTyp read TTyp1;
    property TokenStr: string read GetTokenStr;
    property CurPos: Integer read GetCurPos write SetCurPos;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run; dynamic;
    property Source: string read GetSource write SetSource;
    property VResult: Variant read FVResult;
    property OnGetValue: TJvInterpreterGetValue read FOnGetValue write FOnGetValue;
    property OnSetValue: TJvInterpreterSetValue read FOnSetValue write FOnSetValue;
    property Adapter: TJvInterpreterAdapter read FAdapter;
    property SharedAdapter: TJvInterpreterAdapter read FSharedAdapter;
    property BaseErrLine: Integer read FBaseErrLine write FBaseErrLine;
    property LastError: EJvInterpreterError read FLastError;
  end;

  TParserState = record
    CurPos: Integer;
    Token: Variant;
    TTyp: TTokenTyp;
    PrevTTyp: TTokenTyp;
    Backed: Boolean;
    AllowAssignment: Boolean;
  end;

  TJvInterpreterAddVarFunc = procedure (UnitName: string; Identifer,
    Typ: string; VTyp: Word; const Value: Variant) of object;

 { Function executor }
  TJvInterpreterFunction = class(TJvInterpreterExpression)
  private
    FCurUnitName: string;
    FCurInstance: TObject;
    FBreak, FContinue, FExit: Boolean;
    FunStack: TList;
    FunContext: Pointer; { PFunContext }
    SS: TStrings;
    StateStack: array[0..99] of TParserState;
    StateStackPtr: TStackPtr;
    FEventList: TList;
    function GetLocalVars: TJvInterpreterVarList;
  protected
    procedure Init; override;
    procedure PushState;
    procedure PopState;
    procedure RemoveState;

    procedure InFunction1(FunDesc: TJvInterpreterFunDesc);
    procedure DoOnStatement; virtual;
    procedure Statement1;
    procedure SkipStatement1;
    procedure SkipToEnd1;
    procedure SkipToUntil1;
    procedure SkipIdentifer1;
    procedure FindToken1(TTyp1: TTokenTyp);
    procedure Var1(AddVarFunc: TJvInterpreterAddVarFunc);
    procedure Const1(AddVarFunc: TJvInterpreterAddVarFunc);
    procedure Identifer1;
    procedure Begin1;
    procedure If1;
    procedure While1;
    procedure Repeat1;
    procedure For1;
    procedure Case1;
    procedure Try1;
    procedure Raise1;

    function NewEvent(const UnitName: string; const FunName, EventType: string;
      Instance: TObject): TSimpleEvent;
    procedure InternalSetValue(const Identifer: string);
    function GetValue(Identifer: string; var Value: Variant;
      var Args: TArgs): Boolean; override;
    function SetValue(Identifer: string; const Value: Variant;
      var Args: TArgs): Boolean; override;
    property LocalVars: TJvInterpreterVarList read GetLocalVars;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run; override;
    property CurUnitName: string read FCurUnitName;
    property CurInstance: TObject read FCurInstance;
  end;

  TUnitSection = (usUnknown, usInterface, usImplementation, usInitialization,
    usFinalization);

 { Unit executor }
  TJvInterpreterUnit = class(TJvInterpreterFunction)
  private
    FClearUnits: boolean;
    FEventHandlerList: TList;
    FOnGetUnitSource: TJvInterpreterGetUnitSource;
    FUnitSection: TUnitSection;
  protected
    procedure Init; override;
    procedure ReadFunHeader(FunDesc: TJvInterpreterFunDesc);
    procedure Uses1(var UsesList: string);
    procedure ReadUnit(const UnitName: string);
    procedure Function1;
    procedure Unit1;
    procedure Type1;
    procedure Class1(const Identifer: string);
    function GetValue(Identifer: string; var Value: Variant;
      var Args: TArgs): Boolean; override;
    function SetValue(Identifer: string; const Value: Variant;
      var Args: TArgs): Boolean; override;
    function GetUnitSource(UnitName: string; var Source: string): boolean;
      dynamic;
    procedure ExecFunction(Fun: TJvInterpreterFunDesc);
    procedure SourceChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run; override;
    procedure DeclareExternalFunction(const Declaration: string);
    procedure Compile;
    function CallFunction(const FunName: string; Args: TArgs;
      Params: array of Variant): Variant; override;
    function CallFunctionEx(Instance: TObject; const UnitName: string; 
      const FunName: string; Args: TArgs; Params: array of Variant)
      : Variant; override;
    function FunctionExists(const UnitName: string; const FunName: string)
      : boolean;
    property OnGetUnitSource: TJvInterpreterGetUnitSource read FOnGetUnitSource
      write FOnGetUnitSource;
    property UnitSection: TUnitSection read FUnitSection;
  end;

 { main JvInterpreter component }
  TJvInterpreterProgram = class(TJvInterpreterUnit)
  private
    FPas: TStrings;
    FOnStatement: TNotifyEvent;
    procedure SetPas(Value: TStrings);
  protected
    procedure DoOnStatement; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run; override;
  published
    property Pas: TStrings read FPas write SetPas;
    property OnGetValue;
    property OnSetValue;
    property OnGetUnitSource;
    property OnStatement: TNotifyEvent read FOnStatement write FOnStatement;
  end;

  EJvInterpreterError = class(Exception)
  private
    FExceptionPos: Boolean;
    FErrCode: integer;
    FErrPos: integer;
    FErrName: string;
    FErrName2: string;
    FErrUnitName: string;
    FErrLine: Integer;
    FMessage1: string;
  public
    constructor Create(const AErrCode: integer; const AErrPos: integer;
      const AErrName, AErrName2: string);
    procedure Assign(E: Exception);
    procedure Clear;
    property ErrCode: integer read FErrCode;
    property ErrPos: integer read FErrPos;
    property ErrName: string read FErrName;
    property ErrName2: string read FErrName2;
    property ErrUnitName: string read FErrUnitName;
    property ErrLine: Integer read FErrLine;
    property Message1: string read FMessage1;
  end;

  {Error raising routines}
  procedure JvInterpreterError(const AErrCode: integer; const AErrPos: integer);
  procedure JvInterpreterErrorN(const AErrCode: integer; const AErrPos: integer;
    const AErrName: string);
  procedure JvInterpreterErrorN2(const AErrCode: integer; const AErrPos: integer;
    const AErrName1, AErrName2: string);

  {Utilities functions}
  //function LoadStr2(const ResID: Integer): string;
  function SubStr(const S : string; const index : integer; const Separator : string) : string;

   { RFD - RecordFieldDefinition - return record needed for TJvInterpreterAdapter.AddRec
     Fields parameter }
  function RFD(Identifer: string; Offset: Integer; Typ: Word): TJvInterpreterRecField;

   { raise error ieNotImplemented }
  procedure NotImplemented(Message: string);

   { clear list of TObject }
  procedure ClearList(List: TList);

const
 { additional variant types - TVarData.VType }
  varObject  = $0010;
  varClass   = $0011;
  varPointer = $0012;
  varSet     = $0013;
  varArray   = $0014;
  varRecord  = $0015;


 { V2O - converts variant to object }
function V2O(const V: Variant): TObject;

 { O2V - converts object to variant }
function O2V(O: TObject): Variant;

 { V2C - converts variant to class }
function V2C(const V: Variant): TClass;

 { O2V - converts class to variant }
function C2V(C: TClass): Variant;

 { V2P - converts variant to pointer }
function V2P(const V: Variant): Pointer;

 { P2V - converts pointer to variant }
function P2V(P: Pointer): Variant;

 { R2V - create record holder and put it into variant }
function R2V(ARecordType: string; ARec: Pointer): Variant;

 { V2R - returns pointer to record from variant, containing record holder }
function V2R(const V: Variant): Pointer;

 { P2R - returns pointer to record from record holder, typically Args.Obj }
function P2R(const P: Pointer): Pointer;

 { S2V - converts integer to set and put it into variant }
function S2V(const I: Integer): Variant;

 { V2S - give a set from variant and converts it to integer }
function V2S(V: Variant): Integer;

procedure V2OA(V: Variant; var OA: TOpenArray; var OAValues: TValueArray;
  var Size: Integer);

function TypeName2VarTyp(TypeName: string): Word;

 { copy variant variable with all JvInterpreter variant extension }
procedure JvInterpreterVarCopy(var Dest: Variant; const Source: Variant);

function JvInterpreterVarAsType(const V : Variant; const VarType : integer) : Variant;

 { properly free var variable and set it content to empty }
procedure JvInterpreterVarFree(var V: Variant);

 { compare strings }
function Cmp(const S1, S2 : string) : boolean;

var
  GlobalJvInterpreterAdapter: TJvInterpreterAdapter = nil;

const

  prArgsNoCheck = -1;
  noInstance = HINST(0);
  RFDNull: TJvInterpreterRecField = (Identifer: ''; Offset: 0; Typ: 0);

 {JvInterpreter error codes}

  ieOk = 0; { Okay - no errors }
  ieUnknown = 1;
  ieInternal = 2;
  ieUserBreak = 3; { internal }
  ieRaise = 4; { internal }
  ieErrorPos = 5;
  ieExternal = 6; { non-interpreter error }
  ieAccessDenied = 7;

  { register-time errors }
  ieRegisterBase = 30;
  ieRecordNotDefined = ieRegisterBase + 1;

  { run-time errors }
  ieRuntimeBase = 50;
  ieStackOverFlow = ieRuntimeBase + 2;
  ieTypeMistmatch = ieRuntimeBase + 3;
  ieIntegerOverflow = ieRuntimeBase + 4;
  ieMainUndefined = ieRuntimeBase + 5;
  ieUnitNotFound = ieRuntimeBase + 6;
  ieEventNotRegistered = ieRuntimeBase + 7;
  ieDfmNotFound = ieRuntimeBase + 8;

  { syntax errors (now run-timed) }
  ieSyntaxBase = 100;
  ieBadRemark = ieSyntaxBase + 1; { Bad remark - detected by parser }
  ieIdentiferExpected = ieSyntaxBase + 2;
  ieExpected = ieSyntaxBase + 3;
  ieUnknownIdentifer = ieSyntaxBase + 4;
  ieBooleanRequired = ieSyntaxBase + 5;
  ieClassRequired = ieSyntaxBase + 6;
  ieNotAllowedBeforeElse = ieSyntaxBase + 7;
  ieIntegerRequired = ieSyntaxBase + 8;
  ieROCRequired = ieSyntaxBase + 9;
  ieMissingOperator = ieSyntaxBase + 10;
  ieIdentiferRedeclared = ieSyntaxBase + 11;

 { array indexes }
  ieArrayBase = 170;
  ieArrayIndexOutOfBounds = ieArrayBase + 1;
  ieArrayTooManyParams = ieArrayBase + 2;
  ieArrayNotEnoughParams = ieArrayBase + 3;
  ieArrayBadDimension= ieArrayBase + 4;
  ieArrayBadRange = ieArrayBase + 5;
  ieArrayRequired = ieArrayBase + 6;

 { function call errors (now run-timed) }
  ieFunctionBase = 180;
  ieTooManyParams = ieFunctionBase + 1;
  ieNotEnoughParams = ieFunctionBase + 2;
  ieIncompatibleTypes = ieFunctionBase + 3;
  ieDllErrorLoadLibrary = ieFunctionBase + 4;
  ieDllInvalidArgument = ieFunctionBase + 5;
  ieDllInvalidResult = ieFunctionBase + 6;
  ieDllFunctionNotFound = ieFunctionBase + 7;
  ieDirectInvalidArgument = ieFunctionBase + 8;
  ieDirectInvalidResult = ieFunctionBase + 9;
  ieDirectInvalidConvention = ieFunctionBase + 10;


{$IFDEF JvInterpreter_OLEAUTO}
  ieOleAuto = ieFunctionBase + 21;
{$ENDIF}

  ieUserBase = $300;

  irExpression = 301;
  irIdentifer = 302;
  irDeclaration = 303;
  irEndOfFile = 304;
  irClass = 305;

implementation

uses TypInfo, JvInterpreterConst
{$IFNDEF COMPILER3_UP}
  , Ole2 { IUnknown in Delphi 2 }
{$ENDIF}
{$IFDEF JvInterpreter_OLEAUTO}
  , OleConst
{$IFDEF COMPILER3_UP}
  , ActiveX, ComObj
{$ELSE}
  , OleAuto
{$ENDIF COMPILER3_UP}
{$ENDIF JvInterpreter_OLEAUTO}
  ;


{$R JvInterpreter.res} { error messages }


{ internal structures }
type

 { Adapter classes - translates data from JvInterpreter calls to Delphi functions }

  TJvInterpreterSrcUnit = class(TJvInterpreterIdentifer)
  private
    Source: string;
    UsesList: TNameArray;
  end;

  TParamCount = - 1..MaxArgs;

  TJvInterpreterMethod = class(TJvInterpreterIdentifer)
  private
    ClassType: TClass;
    ParamCount: TParamCount;
    ParamTypes: TTypeArray; { varInteger, varString, .. }
    ResTyp: Word; { varInteger, varString, .. }
    Func: Pointer; { TJvInterpreterAdapterGetValue or TJvInterpreterAdapterSetValue }
  end;

  TJvInterpreterDMethod = class(TJvInterpreterMethod)
  private
    ResTyp: Word;
    CallConvention: TCallConvention;
  end;

  TJvInterpreterClass = class(TJvInterpreterIdentifer)
  private
    ClassType: TClass;
  end;

  TJvInterpreterConst = class(TJvInterpreterIdentifer)
  private
    Value: Variant;
  end;

  TJvInterpreterRecFields = array[0..MaxRecFields] of TJvInterpreterRecField;

  TJvInterpreterRecord = class(TJvInterpreterIdentifer)
  private
    RecordSize: Integer; { sizeof(Rec^) }
    FieldCount: Integer;
    Fields: TJvInterpreterRecFields;
    CreateFunc: TJvInterpreterAdapterNewRecord;
    DestroyFunc: TJvInterpreterAdapterDisposeRecord;
    CopyFunc: TJvInterpreterAdapterCopyRecord;
  end;

  TJvInterpreterRecMethod = class(TJvInterpreterIdentifer)
  private
    JvInterpreterRecord: TJvInterpreterRecord;
    ParamCount: TParamCount;
    ParamTypes: TTypeArray; { varInteger, varString and so one .. }
    ResTyp: Word; { varInteger, varString, .. }
    Func: Pointer; { TJvInterpreterAdapterGetValue or TJvInterpreterAdapterSetValue }
  end;

  TJvInterpreterRecHolder = class(TJvInterpreterIdentifer)
  private
    RecordType: string;
    JvInterpreterRecord: TJvInterpreterRecord;
    Rec: Pointer; { data }
  public
    constructor Create(ARecordType: string; ARec: Pointer);
    destructor Destroy; override;
  end;

  PMethod = ^TMethod;

 { interpreter function }
  TJvInterpreterSrcFun = class(TJvInterpreterIdentifer)
  private
    FunDesc: TJvInterpreterFunDesc;
  public
    constructor Create;
    destructor Destroy; override;
  end;

 { external function }
  TJvInterpreterExtFun = class(TJvInterpreterSrcFun)
  private
    DllInstance: HINST;
    DllName: string;
    FunName: string;
     {or}
    FunIndex: integer;
    function CallDll(Args: TArgs): Variant;
  end;

 { function context - stack }
  PFunContext = ^TFunContext;
  TFunContext = record
    PrevFunContext: PFunContext;
    LocalVars: TJvInterpreterVarList;
    Fun: TJvInterpreterSrcFun;
  end;

  TJvInterpreterEventDesc = class(TJvInterpreterIdentifer)
  private
    EventClass: TJvInterpreterEventClass;
    Code: Pointer;
  end;


{$IFDEF COMPILER2}
 { TJvStringStream  - reduced implementation from Delphi 3 classes.pas }
  TJvStringStream  = class(TStream)
  private
    FDataString: string;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint);
  public
    constructor Create(const AString: string);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  PDouble = ^Double;
  PSmallInt = ^SmallInt;
{$ENDIF COMPILER2}

{$IFDEF COMPLIB_CLX}
type
  DWORD = longint;
  PBool = PBoolean;
{$ENDIF COMPLIB_CLX}

{$IFDEF JvInterpreter_DEBUG}
var
  ObjCount: Integer = 0;
{$ENDIF}


function LoadStr2(const ResID: Integer): string;
var
  i: Integer;
begin
  for i := Low(JvInterpreterErrors) to High(JvInterpreterErrors) do
    if JvInterpreterErrors[i].ID = ResID then
    begin
      Result := JvInterpreterErrors[i].Description;
      Break;
    end;
end;

procedure JvInterpreterError(const AErrCode: integer; const AErrPos: integer);
begin
  raise EJvInterpreterError.Create(AErrCode, AErrPos, '', '');
end; { JvInterpreterError }

procedure JvInterpreterErrorN(const AErrCode: integer; const AErrPos: integer;
  const AErrName: string);
begin
  raise EJvInterpreterError.Create(AErrCode, AErrPos, AErrName, '');
end; { JvInterpreterErrorN }

procedure JvInterpreterErrorN2(const AErrCode: integer; const AErrPos: integer;
  const AErrName1, AErrName2: string);
begin
  raise EJvInterpreterError.Create(AErrCode, AErrPos, AErrName1, AErrName2);
end; { JvInterpreterErrorN2 }

constructor EJvInterpreterError.Create(const AErrCode: integer;
  const AErrPos: integer; const AErrName, AErrName2: string);
begin
  inherited Create('');
  FErrCode := AErrCode;
  FErrPos := AErrPos;
  FErrName := AErrName;
  FErrName2 := AErrName2;
  { function LoadStr don't work sometimes :-( }
  Message := Format(LoadStr2(ErrCode), [ErrName, ErrName2]);
  FMessage1 := Message;
end; { Create }

procedure EJvInterpreterError.Assign(E: Exception);
begin
  Message := E.Message;
  if E is EJvInterpreterError then
  begin
    FErrCode := (E as EJvInterpreterError).FErrCode;
    FErrPos := (E as EJvInterpreterError).FErrPos;
    FErrName := (E as EJvInterpreterError).FErrName;
    FErrName2 := (E as EJvInterpreterError).FErrName2;
    FMessage1 := (E as EJvInterpreterError).FMessage1;
  end;
end;

procedure EJvInterpreterError.Clear;
begin
  FExceptionPos := False;
  FErrName := '';
  FErrName2 := '';
  FErrPos := -1;
  FErrLine := -1;
  FErrUnitName := '';
end;

function V2O(const V: Variant): TObject;
begin
  Result := TVarData(V).vPointer;
end;

function O2V(O: TObject): Variant;
begin
  TVarData(Result).VType := varObject;
  TVarData(Result).vPointer := O;
end;

function V2C(const V: Variant): TClass;
begin
  Result := TVarData(V).vPointer;
end;

function C2V(C: TClass): Variant;
begin
  TVarData(Result).VType := varClass;
  TVarData(Result).vPointer := C;
end;

function V2P(const V: Variant): Pointer;
begin
  Result := TVarData(V).vPointer;
end;

function P2V(P: Pointer): Variant;
begin
  TVarData(Result).VType := varPointer;
  TVarData(Result).vPointer := P;
end;

function R2V(ARecordType: string; ARec: Pointer): Variant;
begin
  TVarData(Result).vPointer := TJvInterpreterRecHolder.Create(ARecordType, ARec);
  TVarData(Result).VType := varRecord;
end;

function V2R(const V: Variant): Pointer;
begin
  if (TVarData(V).VType <> varRecord) or
    not (TObject(TVarData(V).vPointer) is TJvInterpreterRecHolder) then
    JvInterpreterError(ieROCRequired, -1);
  Result := TJvInterpreterRecHolder(TVarData(V).vPointer).Rec;
end;

function P2R(const P: Pointer): Pointer;
begin
  if not (TObject(P) is TJvInterpreterRecHolder) then
    JvInterpreterError(ieROCRequired, -1);
  Result := TJvInterpreterRecHolder(P).Rec;
end;

function S2V(const I: Integer): Variant;
begin
  Result := I;
  TVarData(Result).VType := varSet;
end;

function V2S(V: Variant): Integer;
var
  i: Integer;
begin
  if (TVarData(V).VType and System.varArray) = 0 then
    Result := TVarData(V).VInteger
  else
  begin
   { JvInterpreter thinks about all function parameters, started
     with '[' symbol that they are open arrays;
     but it may be set constant, so we must convert it now }
    Result := 0;
    for i := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
      Result := Result or 1 shl Integer(V[i]);
  end;
end;

procedure V2OA(V: Variant; var OA: TOpenArray; var OAValues: TValueArray;
  var Size: Integer);
var
  i: integer;
begin
  if (TVarData(V).VType and varArray) = 0 then
    JvInterpreterError(ieTypeMistmatch, -1);
  Size := VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1;
  for i := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do { Iterate }
  begin
    case TVarData(V[i]).VType of { }
      varInteger, varSmallInt:
        begin
          OA[i].vInteger := V[i];
          OA[i].VType := vtInteger;
        end;
      varString, varOleStr:
        begin
         // OA[i].vPChar := PChar(string(V[i]));
         // OA[i].VType := vtPChar;
          OAValues[i] := V[i];
          OA[i].vVariant := @OAValues[i];
          OA[i].VType := vtVariant;
        end;
      varBoolean:
        begin
          OA[i].vBoolean := V[i];
          OA[i].VType := vtBoolean;
        end;
      varDouble, varCurrency:
        begin
          OA[i].vExtended := TVarData(V[i]).vPointer;
          OA[i].VType := vtExtended;
        end;
    else
      begin
        OAValues[i] := V[i];
        OA[i].vVariant := @OAValues[i];
        OA[i].VType := vtVariant;
      end;
    end; { case }
  end;
end;

function RFD(Identifer: string; Offset: Integer; Typ: Word)
  : TJvInterpreterRecField;
begin
  Result.Identifer := Identifer;
  Result.Offset := Offset;
  Result.Typ := Typ;
end;

procedure NotImplemented(Message: string);
begin
  JvInterpreterErrorN(ieInternal, -1,
    Message + ' not implemented');
end; { ErrorNotImplemented }

//RWare: added check for "char", otherwise function with ref variable 
//of type char causes AV, like KeyPress event handler
function TypeName2VarTyp(TypeName: string): Word;
begin
  if Cmp(TypeName, 'integer') or Cmp(TypeName, 'longint') or
     Cmp(TypeName, 'dword')then
    Result := varInteger
  else if Cmp(TypeName, 'word') or Cmp(TypeName, 'smallint') then
    Result := varSmallInt
  else if Cmp(TypeName, 'byte') then
    Result := varByte
  else if Cmp(TypeName, 'wordbool') or Cmp(TypeName, 'boolean') or
     Cmp(TypeName, 'bool') then
    Result := varBoolean
  else if Cmp(TypeName, 'string') or Cmp(TypeName, 'PChar') or
    Cmp(TypeName, 'ANSIString') or Cmp(TypeName, 'ShortString')
      or Cmp(TypeName, 'char') then {+RWare}
    Result := varString
  else if Cmp(TypeName, 'double') then
    Result := varDouble
  else if Cmp(TypeName, 'tdatetime') then
    Result := varDate
  else if Cmp(TypeName, 'tobject') then
    Result := varObject
  else
    Result := varEmpty;
end; { TypeName2VarTyp }

procedure ClearList(List: TList);
var
  i: Integer;
begin
  if not Assigned(List) then Exit;
  for i := 0 to List.Count - 1 do { Iterate }
    TObject(List[i]).Free;
  List.Clear;
end; { ClearList }

procedure ClearMethodList(List: TList);
var
  i: Integer;
begin
  for i := 0 to List.Count - 1 do { Iterate }
    Dispose(PMethod(List[i]));
  List.Clear;
end; { ClearMethodList }


{$IFNDEF COMPILER3_UP}

constructor TJvStringStream .Create(const AString: string);
begin
  inherited Create;
  FDataString := AString;
end;

function TJvStringStream .Read(var Buffer; Count: Longint): Longint;
begin
  Result := Length(FDataString) - FPosition;
  if Result > Count then Result := Count;
  Move(PChar(@FDataString[FPosition + 1])^, Buffer, Result);
  Inc(FPosition, Result);
end;

function TJvStringStream .Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
  SetLength(FDataString, (FPosition + Result));
  Move(Buffer, PChar(@FDataString[FPosition + 1])^, Result);
  Inc(FPosition, Result);
end;

function TJvStringStream .Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := Length(FDataString) - Offset;
  end;
  Result := FPosition;
end;

procedure TJvStringStream .SetSize(NewSize: Longint);
begin
  SetLength(FDataString, NewSize);
  if FPosition > NewSize then FPosition := NewSize;
end;
{$ENDIF}

{$IFNDEF COMPILER3_UP}
function AnsiStrIComp(S1, S2: PChar): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, -1,
    S2, -1) - 2;
end;

function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    S1, MaxLen, S2, MaxLen) - 2;
end;
{$ENDIF COMPILER3_UP}

{************* Some code from JvUtils unit **************}
function SubStr(const S : string; const index : integer; const Separator : string) : string;
 {Вырезает подстроку. Подстроки разделяются символом Sep}
var
  i : integer;
  pB, pE : PChar;
begin
  Result := '';
  if ((index < 0) or ((index = 0) and (Length(S) > 0) and (S[1] = Separator))) or
    (Length(S) = 0) then exit;
  pB := PChar(S);
  for i := 1 to index do begin
    pB := StrPos(pB, PChar(Separator));
    if pB = nil then exit;
    pB := pB+Length(Separator);
    if pB[0] = #0 then exit;
  end;
  pE := StrPos(pB+1, PChar(Separator));
  if pE = nil then pE := PChar(S)+Length(S);
  if not (ANSIStrLIComp(pB, PChar(Separator), Length(Separator)) = 0) then
    SetString(Result, pB, pE-pB);
end;

function GetLineByPos(const S : string; const Pos : integer) : integer;
var
  i : integer;
begin
  if Length(S) < Pos then Result := -1
  else begin
    i := 1;
    Result := 0;
    while (i <= Pos) do begin
      if S[i] = #13 then inc(Result);
      inc(i);
    end;
  end;
end;

function Cmp(const S1, S2 : string) : boolean;
begin
{$IFDEF COMPLIB_VCL}
  // Direct call to CompareString is faster when ANSICompareText.
  Result := (Length(S1) = Length(S2)) and
    (CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar(S1),
     -1, PChar(S2), -1) = 2);
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
  Result := ANSICompareText(S1, S2) = 0;
{$ENDIF COMPLIB_CLX}
end;    { Cmp }
{################## from JvUtils unit ##################}

{************* Some code from RAStream unit **************}
procedure StringSaveToStream(Stream : TStream; S : string);
var
  L : integer;
  P : PChar;
begin
  L := Length(S);
  Stream.WriteBuffer(L, sizeof(L));
  P := PChar(S);
  Stream.WriteBuffer(P^, L);
end;

function StringLoadFromStream(Stream : TStream) : string;
var
  L : integer;
  P : PChar;
begin
  Stream.ReadBuffer(L, sizeof(L));
  SetLength(Result, L);
  P := PChar(Result);
  Stream.ReadBuffer(P^, L);
end;

procedure IntSaveToStream(Stream : TStream; int : integer);
begin
  Stream.WriteBuffer(int, sizeof(int));
end;

function IntLoadFromStream(Stream : TStream) : integer;
begin
  Stream.ReadBuffer(Result, sizeof(Result));
end;

procedure WordSaveToStream(Stream : TStream; int : Word);
begin
  Stream.WriteBuffer(int, sizeof(int));
end;

function WordLoadFromStream(Stream : TStream) : Word;
begin
  Stream.ReadBuffer(Result, sizeof(Result));
end;

procedure ExtendedSaveToStream(Stream : TStream; Ext : Extended);
begin
  Stream.WriteBuffer(Ext, sizeof(Ext));
end;

function ExtendedLoadFromStream(Stream : TStream) : Extended;
begin
  Stream.ReadBuffer(Result, sizeof(Result));
end;

procedure BoolSaveToStream(Stream : TStream; bool : boolean);
var
  B : integer;
begin
  B := integer(bool);
  Stream.WriteBuffer(B, sizeof(B));
end;

function BoolLoadFromStream(Stream : TStream) : boolean;
var
  B : integer;
begin
  Stream.ReadBuffer(B, sizeof(B));
  Result := boolean(B);
end;
{################## from RAStream unit ##################}


{$IFDEF JvInterpreter_OLEAUTO}
{************* Some code from Delphi's OleAuto unit **************}
const
{$IFDEF COMPILER3_UP}
{ Maximum number of dispatch arguments }

  MaxDispArgs = 64;
{$ENDIF COMPILER3_UP}

{ Special variant type codes }

  varStrArg = $0048;

{ Parameter type masks }

  atVarMask = $3F;
  atTypeMask = $7F;
  atByRef = $80;

{ Call GetIDsOfNames method on the given IDispatch interface }

procedure GetIDsOfNames(Dispatch: IDispatch; Names: PChar;
  NameCount: Integer; DispIDs: PDispIDList);
var
  I, N: Integer;
  Ch: WideChar;
  P: PWideChar;
  NameRefs: array[0..MaxDispArgs - 1] of PWideChar;
  WideNames: array[0..1023] of WideChar;
  R: Integer;
begin
  I := 0;
  N := 0;
  repeat
    P := @WideNames[I];
    if N = 0 then NameRefs[0] := P else NameRefs[NameCount - N] := P;
    repeat
      Ch := WideChar(Names[I]);
      WideNames[I] := Ch;
      Inc(I);
    until Char(Ch) = #0;
    Inc(N);
  until N = NameCount;
{  if Dispatch.GetIDsOfNames(GUID_NULL, @NameRefs, NameCount,
    LOCALE_SYSTEM_DEFAULT, DispIDs) <> 0 then }
  R := Dispatch.GetIDsOfNames(GUID_NULL, @NameRefs, NameCount,
    LOCALE_SYSTEM_DEFAULT, DispIDs);
  if R <> 0 then
{$IFDEF COMPILER3_UP}
    raise EOleError.CreateFmt(SNoMethod, [Names]);
{$ELSE}
    raise EOleError.CreateResFmt(SNoMethod, [Names]);
{$ENDIF COMPILER3_UP}
end;

{ Central call dispatcher }

procedure VarDispInvoke(Result: PVariant; const Dispatch: Pointer;
  Names: PChar; CallDesc: PCallDesc; ParamTypes: Pointer); cdecl;
var
  DispIDs: array[0..MaxDispArgs - 1] of Integer;
begin
  GetIDsOfNames(IDispatch(Dispatch), Names, CallDesc^.NamedArgCount + 1, @DispIDs);
  if Result <> nil then VarClear(Result^);
{$IFDEF COMPILER3_UP}
  DispatchInvoke(IDispatch(Dispatch), CallDesc, @DispIDs, @ParamTypes, Result);
{$ELSE}
  DispInvoke(Dispatch, CallDesc, @DispIDs, @ParamTypes, Result);
{$ENDIF COMPILER3_UP}
end;
{################## from OleAuto unit ##################}
{$ENDIF JvInterpreter_OLEAUTO}

type
  TFunc = procedure; far;
  TiFunc = function: integer; far;
  TfFunc = function: boolean; far;
  TwFunc = function: word; far;

function CallDllIns(Ins: HINST; FuncName: string; Args: TArgs;
  ParamDesc: TTypeArray; ResTyp: Word): Variant;
var
{  Func: procedure; far;
  iFunc: function: integer; far;
  fFunc: function: boolean; far;
  wFunc: function: word; far; }
  Func : TFunc ;
  iFunc: TiFunc;
  fFunc: TfFunc;
  wFunc: TwFunc;
  i: integer;
  Aint: integer;
 // Abyte : byte;
  Aword : word;
  Apointer: pointer;
  Str: string;
begin
  Result := Null;
  Func := GetProcAddress(Ins, PChar(FuncName));
  iFunc := @Func; fFunc := @Func; wFunc := @Func;
  if @Func <> nil then
  begin

    try

      for i := Args.Count - 1 downto 0 do { 'stdcall' call conversion }
      begin
        if (ParamDesc[i] and varByRef) = 0 then
          case ParamDesc[i] of
            varInteger,{ ttByte,} varBoolean:
              begin Aint := Args.Values[i]; asm push Aint end; end;
            varSmallInt:
              begin Aword := Word(Args.Values[i]); asm push Aword end; end;
            varString:
              begin Apointer := PChar(string(Args.Values[i])); asm push Apointer end; end;
            else
              JvInterpreterErrorN(ieDllInvalidArgument, -1, FuncName);
          end
        else
          case ParamDesc[i] and not varByRef of
            varInteger,{ ttByte,} varBoolean:
              begin Apointer := @TVarData(Args.Values[i]).vInteger; asm push Apointer end; end;
            varSmallInt:
              begin Apointer := @TVarData(Args.Values[i]).vSmallInt; asm push Apointer end; end;
            else
              JvInterpreterErrorN(ieDllInvalidArgument, -1, FuncName);
          end
      end;

      case ResTyp of
        varSmallInt:
          Result := wFunc;
        varInteger:
          Result := iFunc;
        varBoolean:
          Result := Boolean(integer(fFunc));
        varEmpty:
          Func;
        else
          JvInterpreterErrorN(ieDllInvalidResult, -1, FuncName);
      end;

    except
      on E: EJvInterpreterError do
        raise E;
      on E: Exception do
      begin
        Str := E.Message;
        UniqueString(Str);
        raise Exception.Create(Str);
      end;
    end;

  end
  else
    JvInterpreterError(ieDllFunctionNotFound, -1);
end;

function CallDll(DllName, FuncName: string; Args: TArgs;
  ParamDesc: TTypeArray; ResTyp: Word): Variant;
var
  Ins: HINST;
  LastError: DWORD;
begin
  Result := false;
  Ins := LoadLibrary(PChar(DllName));
  if Ins = 0 then
    JvInterpreterErrorN(ieDllErrorLoadLibrary, -1, DllName);
  try
    Result := CallDllIns(Ins, FuncName, Args, ParamDesc, ResTyp);
    LastError := GetLastError;
  finally { wrap up }
    FreeLibrary(Ins);
  end; { try/finally }
  SetLastError(LastError);
end;

procedure ConvertParamTypes(InParams: array of Word; var OutParams: TTypeArray);
var
  i: Integer;
begin
  for i := Low(InParams) to High(InParams) do { Iterate }
    OutParams[i] := InParams[i];
end; { ConvertParamTypes }

procedure ConvertParamNames(InParams: array of string;
  var OutParams: TNameArray);
var
  i: Integer;
begin
  for i := Low(InParams) to High(InParams) do { Iterate }
    OutParams[i] := InParams[i];
end; { ConvertParamTypes }

{ ************************* Array support ************************* }

const
  {Max avalaible dimension for arrays}
  JvInterpreter_MAX_ARRAY_DIMENSION = 10;

type

  TJvInterpreterArrayValues = array[0..JvInterpreter_MAX_ARRAY_DIMENSION - 1] of Integer;

  PJvInterpreterArrayRec = ^TJvInterpreterArrayRec;
  TJvInterpreterArrayRec = packed record
    Dimension: Integer; {number of dimensions}
    BeginPos: TJvInterpreterArrayValues; {starting range for all dimensions}
    EndPos: TJvInterpreterArrayValues; {ending range for all dimensions}
    ItemType: Integer; {array type}
    ElementSize: Integer; {size of element in bytes}
    Size: Integer; {number of elements in array}
    Memory: Pointer; {pointer to memory representation of array}
  end;

function GetArraySize(Dimension: Integer; BeginPos,
  EndPos: TJvInterpreterArrayValues): Integer;
var
  A: Integer;
begin
  Result := 1;
  for A := 0 to Dimension - 1 do
  begin
    Result := Result * ((EndPos[A] - BeginPos[A]) + 1);
  end;
end;

{Calculate starting position of element in memory}

function GetArrayOffset(Dimension: Integer; BeginPos, EndPos: TJvInterpreterArrayValues;
  Element: TJvInterpreterArrayValues): Integer;
var
  A: Integer;
  LastDim: Integer;
begin
  Result := 0;
  LastDim := 1;
  for A := 0 to Dimension - 1 do
  begin
    if (Element[A] < BeginPos[A]) or (Element[A] > EndPos[A]) then
      JvInterpreterError(ieArrayIndexOutOfBounds, -1);
    Result := Result + (LastDim * (Element[A] - BeginPos[A]));
    LastDim := LastDim * (EndPos[A] - BeginPos[A] + 1);
  end;
end;

{Allocate memory for new array}

function JvInterpreterArrayInit(const Dimension: Integer; const BeginPos,
  EndPos: TJvInterpreterArrayValues; const ItemType: Integer): PJvInterpreterArrayRec;
var
  PP: PJvInterpreterArrayRec;
  SS: TStringList;
  AA: Integer;
  ArraySize: Integer;
begin
  if (Dimension < 1) or (Dimension > MaxArgs) then
    JvInterpreterError(ieArrayBadDimension, -1);
  for AA := 0 to Dimension - 1 do
  begin
    if not (BeginPos[AA] <= EndPos[AA]) then
      JvInterpreterError(ieArrayBadRange, -1);
  end;

  New(PP);
  PP^.BeginPos := BeginPos;
  PP^.EndPos := EndPos;
  PP^.ItemType := ItemType;
  ArraySize := GetArraySize(Dimension, BeginPos, EndPos);
  PP^.Size := ArraySize;
  PP^.Dimension := Dimension;
  case ItemType of
    varInteger, varObject:
      begin
        PP^.ElementSize := SizeOf(Integer);
      end;
    varDouble:
      begin
        PP^.ElementSize := SizeOf(Double);
      end;
    varByte:
      begin
        PP^.ElementSize := SizeOf(Byte);
      end;
    varSmallInt:
      begin
        PP^.ElementSize := SizeOf(varSmallInt);
      end;
    varDate:
      begin
        PP^.ElementSize := SizeOf(Double);
      end;
    varString:
      begin
        PP^.ElementSize := 0;
        SS := TStringList.Create;
        for AA := 1 to ArraySize do
          SS.Add('');
        PP^.Memory := SS;
      end;
  end;
  if ItemType <> varString then
  begin
    GetMem(PP^.Memory, ArraySize * PP^.ElementSize);
    //ZeroMemory(PP^.Memory, ArraySize * PP^.ElementSize);
    FillChar(PP^.Memory^, ArraySize * PP^.ElementSize, 0);
  end;
  Result := PP;
end;

{Free memory for array}

procedure JvInterpreterArrayFree(JvInterpreterArrayRec: PJvInterpreterArrayRec);
begin
  if not Assigned(JvInterpreterArrayRec) then Exit;
  if (JvInterpreterArrayRec^.ItemType <> varString) then
  begin
    FreeMem(JvInterpreterArrayRec^.Memory, (JvInterpreterArrayRec^.Size) *
      JvInterpreterArrayRec^.ElementSize);
    Dispose(JvInterpreterArrayRec);
  end
  else
  begin
    TStringList(JvInterpreterArrayRec^.Memory).Clear;
    TStringList(JvInterpreterArrayRec^.Memory).Free;
    Dispose(JvInterpreterArrayRec);
  end;
end;

{Set element for array}

procedure JvInterpreterArraySetElement(Element: TJvInterpreterArrayValues; Value: Variant;
  JvInterpreterArrayRec: PJvInterpreterArrayRec);
var
  Offset: Integer;
begin
  if JvInterpreterArrayRec^.Dimension > 1 then
    Offset := GetArrayOffset(JvInterpreterArrayRec^.Dimension, JvInterpreterArrayRec^.BeginPos,
      JvInterpreterArrayRec^.EndPos, Element)
  else
    Offset := Element[0] - JvInterpreterArrayRec^.BeginPos[0];
  case JvInterpreterArrayRec^.ItemType of
    varInteger:
      PInteger(Pointer(Integer(JvInterpreterArrayRec^.Memory) +
        (Offset * JvInterpreterArrayRec^.ElementSize)))^ := Value;
    varDouble:
      PDouble(Pointer(Integer(JvInterpreterArrayRec^.Memory) +
        (Offset * JvInterpreterArrayRec^.ElementSize)))^ := Value;
    varByte:
      PByte(Pointer(Integer(JvInterpreterArrayRec^.Memory) +
        (Offset * JvInterpreterArrayRec^.ElementSize)))^ := Value;
    varSmallInt:
      PSmallInt(Pointer(Integer(JvInterpreterArrayRec^.Memory) +
        (Offset * JvInterpreterArrayRec^.ElementSize)))^ := Value;
    varDate:
      PDouble(Pointer(Integer(JvInterpreterArrayRec^.Memory) +
        (Offset * JvInterpreterArrayRec^.ElementSize)))^ := Value;
    varString:
      begin
        Value := VarAsType(Value, varString);
        TStringList(JvInterpreterArrayRec^.Memory).Strings[Offset] := Value;
      end;
    varObject:
      PInteger(Pointer(Integer(JvInterpreterArrayRec^.Memory) +
        (Offset * JvInterpreterArrayRec^.ElementSize)))^ := TVarData(Value).VInteger;
  end;
end;

{Get element for array}

function JvInterpreterArrayGetElement(Element: TJvInterpreterArrayValues; JvInterpreterArrayRec:
  PJvInterpreterArrayRec): Variant;
var
  Offset: Integer;
begin
  if JvInterpreterArrayRec^.Dimension > 1 then
    Offset := GetArrayOffset(JvInterpreterArrayRec^.Dimension, JvInterpreterArrayRec^.BeginPos,
      JvInterpreterArrayRec^.EndPos, Element)
  else
    Offset := Element[0] - JvInterpreterArrayRec^.BeginPos[0];
  case JvInterpreterArrayRec^.ItemType of
    varInteger:
      Result := Integer(Pointer(Integer(JvInterpreterArrayRec^.Memory) + ((Offset) *
        JvInterpreterArrayRec^.ElementSize))^);
    varDouble:
      Result := Double(Pointer(Integer(JvInterpreterArrayRec^.Memory) + ((Offset) *
        JvInterpreterArrayRec^.ElementSize))^);
    varByte:
      Result := Byte(Pointer(Integer(JvInterpreterArrayRec^.Memory) + ((Offset) *
        JvInterpreterArrayRec^.ElementSize))^);
    varSmallInt:
      Result := SmallInt(Pointer(Integer(JvInterpreterArrayRec^.Memory) + ((Offset) *
        JvInterpreterArrayRec^.ElementSize))^);
    varDate:
      Result := TDateTime(Pointer(Integer(JvInterpreterArrayRec^.Memory) + ((Offset) *
        JvInterpreterArrayRec^.ElementSize))^);
    varString:
      Result := TStringList(JvInterpreterArrayRec^.Memory).Strings[Offset];
    varObject:
      begin
        Result := Integer(Pointer(Integer(JvInterpreterArrayRec^.Memory) + ((Offset) *
          JvInterpreterArrayRec^.ElementSize))^);
        TVarData(Result).VType := varObject;
      end;
  end;
end;

{ ########################## Array support ########################## }

{ ************************ extended variants ************************ }

function JvInterpreterVarAsType(const V : Variant; const VarType : integer) : Variant;
begin
  if TVarData(V).VType in [varEmpty, varNull] then
  begin
    case VarType of
      varString,
      varOleStr    : Result := '';
      varInteger,
      varSmallint,
      varByte      : Result := 0;
      varBoolean   : Result := false;
      varSingle,
      varDouble,
      varCurrency,
      varDate      : Result := 0.0;
      varVariant   : Result := Null;
      else Result := VarAsType(V, VarType);
    end;
  end else
  begin
    case TVarData(V).VType of
      varInteger:
        if (TVarData(V).VType = varType) then
          Result := Integer(V = True)
        else
          Result := Integer(V);
      varArray:
        begin
          TVarData(Result) := TVarData(V);
          TVarData(Result).VType := VarType;
        end;
      else
        Result := VarAsType(V, VarType);
    end;
  end;
end;

procedure JvInterpreterVarCopy(var Dest: Variant; const Source: Variant);
begin
  if TVarData(Source).VType in [varArray, varRecord] then
    TVarData(Dest) := TVarData(Source)
  else
    Dest := Source;
end;

procedure JvInterpreterVarFree(var V: Variant);
begin
  case TVarData(V).VType of
    varArray:
      JvInterpreterArrayFree(PJvInterpreterArrayRec(TVarData(V).vPointer));
    varRecord:
      TJvInterpreterRecHolder(TVarData(V).VPointer).Free;
  end;
  V := Null;
end;

{
function VarAsType2(const V: Variant; VarType: Integer): Variant;
begin
  if TVarData(V).VType = varNull then
    Result := VarAsType(Unassigned,VarType)
  else
    Result := VarAsType(V,VarType);
end;
}

function Var2Type(V : Variant; const VarType : integer) : variant;
begin
  if TVarData(V).VType in [varEmpty, varNull] then
  begin
    case VarType of
      varString,
      varOleStr    : Result := '';
      varInteger,
      varSmallint,
      varByte      : Result := 0;
      varBoolean   : Result := false;
      varSingle,
      varDouble,
      varCurrency,
      varDate      : Result := 0.0;
      varVariant   : Result := Null;
      else Result := VarAsType(V, VarType);
    end;
  end else
    Result := VarAsType(V, VarType);
  if (VarType = varInteger) and (TVarData(V).VType = varBoolean) then
    Result := Integer(V = True);
end;
{ ######################## extended variants ######################## }


{ ************************** TJvInterpreterVar ************************** }
destructor TJvInterpreterVar.Destroy;
begin
  JvInterpreterVarFree(Value);
  inherited Destroy;
end;

{ ************************** TJvInterpreterVarList ************************** }

destructor TJvInterpreterVarList.Destroy;
begin
 {$IFNDEF COMPILER4_UP}
  Clear;
 {$ENDIF}
  inherited Destroy;
end;

procedure TJvInterpreterVarList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TJvInterpreterVar(Items[i]).Free;
  inherited Clear;
end;

procedure TJvInterpreterVarList.AddVar(UnitName, Identifer, Typ: string; VTyp: Word;
  const Value: Variant);
var
  VarRec: TJvInterpreterVar;
begin
  if FindVar(UnitName, Identifer) <> nil then
    JvInterpreterErrorN(ieIdentiferRedeclared, -1, Identifer);
  VarRec := TJvInterpreterVar.Create;
  VarRec.Identifer := Identifer;
  VarRec.UnitName := UnitName;
  JvInterpreterVarCopy(VarRec.Value, Value);
  VarRec.Typ := Typ;
  VarRec.VTyp := VTyp;
  Insert(0, VarRec);
end;

function TJvInterpreterVarList.FindVar(const UnitName, Identifer: string): TJvInterpreterVar;
{ if UnitName = '', any unit allowed}
var
  i: Integer;
begin
  for i := 0 to Count - 1 do { Iterate }
  begin
    Result := TJvInterpreterVar(Items[i]);
    if Cmp(Result.Identifer, Identifer) and
       (Cmp(Result.UnitName, UnitName) or (UnitName = '')) then
      Exit;
  end;
  Result := nil;
end;

procedure TJvInterpreterVarList.DeleteVar(const UnitName, Identifer: string);
var
  i: Integer;
  VarRec: TJvInterpreterVar;
begin
  for i := 0 to Count - 1 do { Iterate }
  begin
    VarRec := TJvInterpreterVar(Items[i]);
    if Cmp(VarRec.Identifer, Identifer) and
       (Cmp(VarRec.UnitName, UnitName) or (UnitName = '')) then
    begin
      JvInterpreterVarFree(VarRec.Value);
      VarRec.Free;
      Delete(i);
      Exit;
    end;
  end;
end; { DeleteVar }

function TJvInterpreterVarList.GetValue(Identifer: string; var Value: Variant;
  Args: TArgs): Boolean;
var
  V: TJvInterpreterVar;
begin
  if (Args.Obj = nil) then
    V := FindVar('', Identifer)
  else if (Args.ObjTyp = varObject) and (Args.Obj is TJvInterpreterSrcUnit) then
    V := FindVar((Args.Obj as TJvInterpreterSrcUnit).Identifer, Identifer)
  else
    V := nil;
  Result := V <> nil;
  if Result then
    JvInterpreterVarCopy(Value, V.Value);
end; { GetValue }

(*
function TJvInterpreterVarList.SetValue(Identifer: string; const Value: Variant;
  Args: TArgs): Boolean;
var
  V: TJvInterpreterVar;
begin
  V := FindVar('', Identifer);
  Result := (V <> nil) and (Args.Obj = nil);
  if Result then
    JvInterpreterVarCopy(V.Value, Value);
end; { SetValue }
*)

function TJvInterpreterVarList.SetValue(Identifer: string; const Value: Variant;
  Args: TArgs): Boolean;
var
  V: TJvInterpreterVar;
begin
  V := FindVar('', Identifer);
  Result := (V <> nil) and (Args.Obj = nil);
  if Result then
    { If 0, then it's probably an object }
    { If a Variant, then we don't care about typecasting }
    { We only want to typecast if the types are not the same, for speed }
    if (V.VTyp<>0) and
       (V.VTyp<>varVariant) and
       (TVarData(Value).VType<>V.VTyp) then
    begin
      { Is it a passed-by-reference variable? }
      if V.VTyp and VarByRef>0 then
      begin
        JvInterpreterVarCopy(V.Value, JvInterpreterVarAsType(Value,V.VTyp and not VarByRef));
        V.VTyp := V.VTyp or VarByRef;
      end
      else
        JvInterpreterVarCopy(V.Value, JvInterpreterVarAsType(Value,V.VTyp))
    end
    else
      JvInterpreterVarCopy(V.Value, Value);
end; { SetValue }

{ ************************** TAdapter ************************** }

 { TJvInterpreterFunDesc }
function TJvInterpreterFunDesc.GetParamType(Index: Integer): Word;
begin
  Result := FParamTypes[Index];
end;

function TJvInterpreterFunDesc.GetParamName(Index: Integer): string;
begin
  Result := FParamNames[Index];
end;

 { TJvInterpreterRecHolder }
constructor TJvInterpreterRecHolder.Create(ARecordType: string; ARec: Pointer);
begin
  RecordType := ARecordType;
  Rec := ARec;
 {$IFDEF JvInterpreter_DEBUG}
  inc(ObjCount);
 {$ENDIF JvInterpreter_DEBUG}
end; { Create }

destructor TJvInterpreterRecHolder.Destroy;
begin
  if Assigned(JvInterpreterRecord) then
  begin
    if Assigned(JvInterpreterRecord.DestroyFunc) then
      JvInterpreterRecord.DestroyFunc(Rec)
    else
      FreeMem(Rec, JvInterpreterRecord.RecordSize);
  end
  else
    JvInterpreterError(ieInternal, -1);
  inherited Destroy;
 {$IFDEF JvInterpreter_DEBUG}
  dec(ObjCount);
 {$ENDIF JvInterpreter_DEBUG}
end;

{ TJvInterpreterSrcFun }
constructor TJvInterpreterSrcFun.Create;
begin
  inherited Create;
  FunDesc := TJvInterpreterFunDesc.Create;
end;

destructor TJvInterpreterSrcFun.Destroy;
begin
  FunDesc.Free;
  inherited Destroy;
end;

 { TJvInterpreterExtFun }
function TJvInterpreterExtFun.CallDll(Args: TArgs): Variant;
begin
  if DllInstance > 0 then
    Result := JvInterpreter.CallDllIns(DllInstance, FunName, Args, FunDesc.FParamTypes,
      FunDesc.ResTyp)
  else
    Result := JvInterpreter.CallDll(DllName, FunName, Args, FunDesc.FParamTypes,
      FunDesc.ResTyp)
end;


{************************ TJvInterpreterEvent ************************}

constructor TJvInterpreterEvent.Create(AOwner: TJvInterpreterExpression; AInstance: TObject;
  AUnitName, AFunName: string);
begin
  Owner := AOwner;
  Instance := AInstance;
  UnitName := AUnitName;
  FunName := AFunName;
 {$IFDEF JvInterpreter_DEBUG}
  inc(ObjCount);
 {$ENDIF JvInterpreter_DEBUG}
end;

destructor TJvInterpreterEvent.Destroy;
begin
  FArgs.Free;
  inherited Destroy;
 {$IFDEF JvInterpreter_DEBUG}
  dec(ObjCount);
 {$ENDIF JvInterpreter_DEBUG}
end; { Destroy }

function TJvInterpreterEvent.GetArgs: TArgs;
begin
  if FArgs = nil then
    FArgs := TArgs.Create;
  Result := FArgs;
end;

function TJvInterpreterEvent.CallFunction(Args: TArgs; Params: array of Variant)
  : Variant;
var
  i: Integer;
  NV: Variant;
begin
  if Args = nil then
    Args := Self.Args;
  Args.Clear;
  for i := Low(Params) to High(Params) do { Iterate }
  begin
    Args.Values[Args.Count] := Params[i];
    inc(Args.Count);
  end; { for }
  NV := Null;
  Result := Owner.CallFunctionEx(Instance, UnitName, FunName, Args, NV);
end;
{######################## TJvInterpreterEvent ########################}

{ TIdentiferList }

function TJvInterpreterIdentiferList.Find(const Identifer: string; var Index: Integer)
  : Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := AnsiStrIComp(PChar(TJvInterpreterIdentifer(List[I]).Identifer), PChar(Identifer));
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

procedure TJvInterpreterIdentiferList.Sort;

  function SortIdentifer(Item1, Item2: Pointer): Integer;
  begin
    { function AnsiStrIComp about 30% faster when AnsiCompareText }
    { Result := AnsiCompareText(TJvInterpreterIdentifer(Item1).Identifer,
       TJvInterpreterIdentifer(Item2).Identifer); }
    Result := AnsiStrIComp(PChar(TJvInterpreterIdentifer(Item1).Identifer),
      PChar(TJvInterpreterIdentifer(Item2).Identifer));
  end;

begin
  inherited Sort(@SortIdentifer);
end;

function TJvInterpreterIdentiferList.IndexOf(const UnitName, Identifer: string)
  : TJvInterpreterIdentifer;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    Result := TJvInterpreterIdentifer(Items[i]);
    if Cmp(Result.Identifer, Identifer) and
      ( Cmp(Result.UnitName, UnitName) or (UnitName = '')) then
      Exit;
  end;
  Result := nil;
end;

{************************ TJvInterpreterAdapter ************************}

constructor TJvInterpreterAdapter.Create(AOwner: TJvInterpreterExpression);
begin
  FOwner := AOwner;
  FSrcUnitList := TJvInterpreterIdentiferList.Create;
  FExtUnitList := TJvInterpreterIdentiferList.Create;
  FGetList := TJvInterpreterIdentiferList.Create;
  FSetList := TJvInterpreterIdentiferList.Create;
  FIGetList := TJvInterpreterIdentiferList.Create;
  FISetList := TJvInterpreterIdentiferList.Create;
  FIDGetList := TJvInterpreterIdentiferList.Create;
  FIDSetList := TJvInterpreterIdentiferList.Create;
  FDGetList := TJvInterpreterIdentiferList.Create;
  FClassList := TJvInterpreterIdentiferList.Create;
  FConstList := TJvInterpreterIdentiferList.Create;
  FFunList := TJvInterpreterIdentiferList.Create;
  FRecList := TJvInterpreterIdentiferList.Create;
  FRecGetList := TJvInterpreterIdentiferList.Create;
  FRecSetList := TJvInterpreterIdentiferList.Create;
  FOnGetList := TJvInterpreterIdentiferList.Create;
  FOnSetList := TJvInterpreterIdentiferList.Create;
  FExtFunList := TJvInterpreterIdentiferList.Create;
  FSrcFunList := TJvInterpreterIdentiferList.Create;
  FEventHandlerList := TJvInterpreterIdentiferList.Create;
  FEventList := TJvInterpreterIdentiferList.Create;
  FSrcVarList := TJvInterpreterVarList.Create;
  FSrcClassList := TJvInterpreterIdentiferList.Create;

  FGetList.Duplicates := dupAccept;
  FSetList.Duplicates := dupAccept;
  FIGetList.Duplicates := dupAccept;
  FISetList.Duplicates := dupAccept;
end;

destructor TJvInterpreterAdapter.Destroy;
begin
  Clear;
  FSrcUnitList.Free;
  FExtUnitList.Free;
  FGetList.Free;
  FSetList.Free;
  FIGetList.Free;
  FISetList.Free;
  FIDGetList.Free;
  FIDSetList.Free;
  FDGetList.Free;
  FClassList.Free;
  FConstList.Free;
  FFunList.Free;
  FRecList.Free;
  FRecGetList.Free;
  FRecSetList.Free;
  FOnGetList.Free;
  FOnSetList.Free;
  FExtFunList.Free;
  FSrcFunList.Free;
  FEventHandlerList.Free;
  FEventList.Free;
  FSrcVarList.Free;
  FSrcClassList.Free;
  inherited Destroy;
end;

{ clear source }

procedure TJvInterpreterAdapter.ClearSource;
begin
  ClearList(FSrcUnitList);
  ClearList(FSrcFunList);
  FSrcVarList.Clear;
  ClearList(FSrcClassList);
end;

{ clear all except source }

procedure TJvInterpreterAdapter.ClearNonSource;
begin
  ClearList(FExtUnitList);
  ClearList(FGetList);
  ClearList(FSetList);
  ClearList(FIGetList);
  ClearList(FISetList);
  ClearList(FIDGetList);
  ClearList(FIDSetList);
  ClearList(FDGetList);
  ClearList(FClassList);
  ClearList(FConstList);
  ClearList(FFunList);
  ClearList(FRecList);
  ClearList(FRecGetList);
  ClearList(FRecSetList);
  ClearList(FExtFunList);
  ClearList(FEventHandlerList);
  ClearList(FEventList);
  ClearMethodList(FOnGetList);
  ClearMethodList(FOnSetList);
end;

{ clear all }

procedure TJvInterpreterAdapter.Clear;
begin
  ClearSource;
  ClearNonSource;
end;

procedure TJvInterpreterAdapter.Assign(Source: TJvInterpreterAdapter);
var
  i: Integer;
begin
  if Source = Self then Exit;

  for i := 0 to Source.FGetList.Count - 1 do { Iterate }
    with TJvInterpreterMethod(Source.FGetList[i]) do
      AddGetEx(ClassType, Identifer, Func, ParamCount, ParamTypes, ResTyp, Data);
  for i := 0 to Source.FSetList.Count - 1 do { Iterate }
    with TJvInterpreterMethod(Source.FSetList[i]) do
      AddSetEx(ClassType, Identifer, Func, ParamCount, ParamTypes, Data);
  for i := 0 to Source.FIGetList.Count - 1 do { Iterate }
    with TJvInterpreterMethod(Source.FIGetList[i]) do
      AddIGetEx(ClassType, Identifer, Func, ParamCount, ParamTypes, ResTyp, Data);
  for i := 0 to Source.FISetList.Count - 1 do { Iterate }
    with TJvInterpreterMethod(Source.FISetList[i]) do
      AddISetEx(ClassType, Identifer, Func, ParamCount, ParamTypes, Data);
  for i := 0 to Source.FIDGetList.Count - 1 do { Iterate }
    with TJvInterpreterMethod(Source.FIDGetList[i]) do
      AddIDGetEx(ClassType, Func, ParamCount, ParamTypes, ResTyp, Data);
  for i := 0 to Source.FIDSetList.Count - 1 do { Iterate }
    with TJvInterpreterMethod(Source.FIDSetList[i]) do
      AddIDSetEx(ClassType, Func, ParamCount, ParamTypes, Data);
  for i := 0 to Source.FDGetList.Count - 1 do { Iterate }
    with TJvInterpreterDMethod(Source.FDGetList[i]) do
      AddDGetEx(ClassType, Identifer, Func, ParamCount, ParamTypes, ResTyp,
        CallConvention, Data);
  for i := 0 to Source.FFunList.Count - 1 do { Iterate }
    with TJvInterpreterMethod(Source.FFunList[i]) do
      AddFunEx(UnitName, Identifer, Func, ParamCount, ParamTypes, ResTyp, Data);
  for i := 0 to Source.FExtUnitList.Count - 1 do { Iterate }
    with TJvInterpreterIdentifer(Source.FExtUnitList[i]) do
      AddExtUnitEx(Identifer, Data);
  for i := 0 to Source.FClassList.Count - 1 do { Iterate }
    with TJvInterpreterClass(Source.FClassList[i]) do
      AddClassEx(UnitName, ClassType, Identifer, Data);
  for i := 0 to Source.FConstList.Count - 1 do { Iterate }
    with TJvInterpreterConst(Source.FConstList[i]) do
      AddConstEx(UnitName, Identifer, Value, Data);
  for i := 0 to Source.FRecList.Count - 1 do { Iterate }
    with TJvInterpreterRecord(Source.FRecList[i]) do
      AddRecEx(UnitName, Identifer, RecordSize, Fields, CreateFunc,
        DestroyFunc, CopyFunc, Data);
  for i := 0 to Source.FRecGetList.Count - 1 do { Iterate }
    with TJvInterpreterRecMethod(Source.FRecGetList[i]) do
      AddRecGetEx(UnitName, JvInterpreterRecord.Identifer, Identifer, Func, ParamCount,
        ParamTypes, ResTyp, Data);
  for i := 0 to Source.FRecSetList.Count - 1 do { Iterate }
    with TJvInterpreterRecMethod(Source.FRecSetList[i]) do
      AddRecSetEx(UnitName, JvInterpreterRecord.Identifer, Identifer, Func, ParamCount,
        ParamTypes, Data);
  for i := 0 to Source.FExtFunList.Count - 1 do { Iterate }
    with TJvInterpreterExtFun(Source.FExtFunList[i]) do
      AddExtFunEx(UnitName, Identifer, DllInstance, DllName, FunName, FunIndex,
        FunDesc.FParamCount, FunDesc.FParamTypes, FunDesc.FResTyp, Data);
  for i := 0 to Source.FEventHandlerList.Count - 1 do { Iterate }
    with TJvInterpreterEventDesc(Source.FEventHandlerList[i]) do
      AddHandlerEx(UnitName, Identifer, EventClass, Code, Data);
  for i := 0 to Source.FEventList.Count - 1 do { Iterate }
    with TJvInterpreterClass(Source.FEventList[i]) do
      AddEventEx(UnitName, ClassType, Identifer, Data);
  for i := 0 to Source.FOnGetList.Count - 1 do { Iterate }
    AddOnGet(TJvInterpreterGetValue(PMethod(Source.FOnGetList[i])^));
  for i := 0 to Source.FOnSetList.Count - 1 do { Iterate }
    AddOnSet(TJvInterpreterSetValue(PMethod(Source.FOnSetList[i])^));
end;

procedure TJvInterpreterAdapter.AddSrcUnit(Identifer: string; Source: string;
  UsesList: string);
begin
  AddSrcUnitEx(Identifer, Source, UsesList, nil);
end;

{ if unit with name 'Identifer' allready exists its source will be replaced }
procedure TJvInterpreterAdapter.AddSrcUnitEx(Identifer: string; Source: string;
  UsesList: string; Data: Pointer);
var
  JvInterpreterUnit: TJvInterpreterSrcUnit;
  S: string;
  i: Integer;
  JvInterpreterIdentifer: TJvInterpreterIdentifer;
begin
  JvInterpreterUnit := nil;
  for i := 0 to FSrcUnitList.Count - 1 do { Iterate }
  begin
    JvInterpreterIdentifer := TJvInterpreterIdentifer(FSrcUnitList.Items[i]);
    if Cmp(JvInterpreterIdentifer.Identifer, Identifer) then
    begin
      JvInterpreterUnit := TJvInterpreterSrcUnit(FSrcUnitList.Items[i]);
      Break;
    end;
  end; { for }
  if JvInterpreterUnit = nil then
  begin
    JvInterpreterUnit := TJvInterpreterSrcUnit.Create;
    FSrcUnitList.Add(JvInterpreterUnit);
  end;

  if JvInterpreterUnit.Source = '' then
  begin
    JvInterpreterUnit.Identifer := Identifer;
    JvInterpreterUnit.Source := Source;
    JvInterpreterUnit.Data := Data;
    i := 0;
    S := Trim(SubStr(UsesList, i, ','));
    while S <> '' do
    begin
      JvInterpreterUnit.UsesList[i] := S;
      inc(i);
      S := Trim(SubStr(UsesList, i, ','));
    end; { while }
  end;
end;

procedure TJvInterpreterAdapter.AddExtUnit(Identifer: string);
begin
  AddExtUnitEx(Identifer, nil);
end;

procedure TJvInterpreterAdapter.AddExtUnitEx(Identifer: string; Data: Pointer);
var
  JvInterpreterUnit: TJvInterpreterIdentifer;
begin
  JvInterpreterUnit := TJvInterpreterIdentifer.Create;
  JvInterpreterUnit.Identifer := Identifer;
  JvInterpreterUnit.Data := Data;
  FExtUnitList.Add(JvInterpreterUnit);
end;

procedure TJvInterpreterAdapter.AddClass(UnitName: string; ClassType: TClass;
  Identifer: string);
begin
  AddClassEx(UnitName, ClassType, Identifer, nil);
end;

procedure TJvInterpreterAdapter.AddClassEx(UnitName: string; ClassType: TClass;
  Identifer: string; Data: Pointer);
var
  JvInterpreterClass: TJvInterpreterClass;
begin
  JvInterpreterClass := TJvInterpreterClass.Create;
  JvInterpreterClass.ClassType := ClassType;
  JvInterpreterClass.Identifer := Identifer;
  JvInterpreterClass.Data := Data;
  FClassList.Add(JvInterpreterClass);
  FSorted:=false;  // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddGet(ClassType: TClass; Identifer: string;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word);
begin
  AddGetEx(ClassType, Identifer, GetFunc, ParamCount, ParamTypes, ResTyp, nil);
end;

procedure TJvInterpreterAdapter.AddGetEx(ClassType: TClass; Identifer: string;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word; Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterMethod;
begin
  JvInterpreterMethod := TJvInterpreterMethod.Create;
  JvInterpreterMethod.ClassType := ClassType;
  JvInterpreterMethod.Identifer := Identifer;
  JvInterpreterMethod.Func := @GetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.ResTyp := ResTyp;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FGetList.Add(JvInterpreterMethod);
  FSorted:=false;  // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddIGet(ClassType: TClass; Identifer: string;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word);
begin
  AddIGetEx(ClassType, Identifer, GetFunc, ParamCount, ParamTypes, ResTyp, nil);
end;

procedure TJvInterpreterAdapter.AddIGetEx(ClassType: TClass; Identifer: string;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word; Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterMethod;
begin
  JvInterpreterMethod := TJvInterpreterMethod.Create;
  JvInterpreterMethod.ClassType := ClassType;
  JvInterpreterMethod.Identifer := Identifer;
  JvInterpreterMethod.Func := @GetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.ResTyp := ResTyp;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FIGetList.Add(JvInterpreterMethod);
  FSorted:=false;  // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddIDGet(ClassType: TClass;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word);
begin
  AddIDGetEx(ClassType, GetFunc, ParamCount, ParamTypes, ResTyp, nil);
end;

procedure TJvInterpreterAdapter.AddIDGetEx(ClassType: TClass;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word; Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterMethod;
begin
  JvInterpreterMethod := TJvInterpreterMethod.Create;
  JvInterpreterMethod.ClassType := ClassType;
  JvInterpreterMethod.Func := @GetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.ResTyp := ResTyp;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FIDGetList.Add(JvInterpreterMethod);
end;

procedure TJvInterpreterAdapter.AddDGet(ClassType: TClass; Identifer: string;
  GetFunc: Pointer; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word; CallConvention: TCallConvention);
begin
  AddDGetEx(ClassType, Identifer, GetFunc, ParamCount, ParamTypes, ResTyp,
    CallConvention, nil);
end;

procedure TJvInterpreterAdapter.AddDGetEx(ClassType: TClass; Identifer: string;
  GetFunc: Pointer; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word; CallConvention: TCallConvention; Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterDMethod;
begin
  JvInterpreterMethod := TJvInterpreterDMethod.Create;
  JvInterpreterMethod.ClassType := ClassType;
  JvInterpreterMethod.Identifer := Identifer;
  JvInterpreterMethod.Func := GetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.ResTyp := ResTyp;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  JvInterpreterMethod.CallConvention := CallConvention;
  FDGetList.Add(JvInterpreterMethod);
end;

procedure TJvInterpreterAdapter.AddSet(ClassType: TClass; Identifer: string;
  SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer; ParamTypes: array of Word);
begin
  AddSetEx(ClassType, Identifer, SetFunc, ParamCount, ParamTypes, nil);
end;

procedure TJvInterpreterAdapter.AddSetEx(ClassType: TClass; Identifer: string;
  SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer; ParamTypes: array of Word;
  Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterMethod;
begin
  JvInterpreterMethod := TJvInterpreterMethod.Create;
  JvInterpreterMethod.ClassType := ClassType;
  JvInterpreterMethod.Identifer := Identifer;
  JvInterpreterMethod.Func := @SetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FSetList.Add(JvInterpreterMethod);
  FSorted:=false;  // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddISet(ClassType: TClass; Identifer: string;
  SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer; ParamTypes: array of Word);
begin
  AddISetEx(ClassType, Identifer, SetFunc, ParamCount, ParamTypes, nil);
end;

procedure TJvInterpreterAdapter.AddISetEx(ClassType: TClass; Identifer: string;
  SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer; ParamTypes: array of Word;
  Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterMethod;
begin
  JvInterpreterMethod := TJvInterpreterMethod.Create;
  JvInterpreterMethod.ClassType := ClassType;
  JvInterpreterMethod.Identifer := Identifer;
  JvInterpreterMethod.Func := @SetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FISetList.Add(JvInterpreterMethod);
  FSorted:=false;  // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddIDSet(ClassType: TClass;
  SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer; ParamTypes: array of Word);
begin
  AddIDSetEx(ClassType, SetFunc, ParamCount, ParamTypes, nil);
end;

procedure TJvInterpreterAdapter.AddIDSetEx(ClassType: TClass;
  SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer; ParamTypes: array of Word;
  Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterMethod;
begin
  JvInterpreterMethod := TJvInterpreterMethod.Create;
  JvInterpreterMethod.ClassType := ClassType;
  JvInterpreterMethod.Func := @SetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FIDSetList.Add(JvInterpreterMethod);
end;

procedure TJvInterpreterAdapter.AddFun(UnitName: string; Identifer: string;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word);
begin
  AddFunEx(UnitName, Identifer, GetFunc, ParamCount, ParamTypes, ResTyp, nil);
end;

procedure TJvInterpreterAdapter.AddFunEx(UnitName: string; Identifer: string;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word; Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterMethod;
begin
  JvInterpreterMethod := TJvInterpreterMethod.Create;
  JvInterpreterMethod.Identifer := Identifer;
  JvInterpreterMethod.Func := @GetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.ResTyp := ResTyp;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FFunList.Add(JvInterpreterMethod);
  FSorted:=false;  // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddRec(UnitName: string; Identifer: string;
  RecordSize: Integer; Fields: array of TJvInterpreterRecField;
  CreateFunc: TJvInterpreterAdapterNewRecord; DestroyFunc: TJvInterpreterAdapterDisposeRecord;
  CopyFunc: TJvInterpreterAdapterCopyRecord);
begin
  AddRecEx(UnitName, Identifer, RecordSize, Fields, CreateFunc, DestroyFunc,
    CopyFunc, nil);
end;

procedure TJvInterpreterAdapter.AddRecEx(UnitName: string; Identifer: string;
  RecordSize: Integer; Fields: array of TJvInterpreterRecField;
  CreateFunc: TJvInterpreterAdapterNewRecord; DestroyFunc: TJvInterpreterAdapterDisposeRecord;
  CopyFunc: TJvInterpreterAdapterCopyRecord; Data: Pointer);
var
  JvInterpreterRecord: TJvInterpreterRecord;
  i: Integer;
begin
  JvInterpreterRecord := TJvInterpreterRecord.Create;
  JvInterpreterRecord.Identifer := Identifer;
  JvInterpreterRecord.RecordSize := RecordSize;
  JvInterpreterRecord.CreateFunc := CreateFunc;
  JvInterpreterRecord.DestroyFunc := DestroyFunc;
  JvInterpreterRecord.CopyFunc := CopyFunc;
  JvInterpreterRecord.Data := Data;
  for i := Low(Fields) to High(Fields) do { Iterate }
    JvInterpreterRecord.Fields[i] := Fields[i];
  JvInterpreterRecord.FieldCount := High(Fields) - Low(Fields) + 1;
  FRecList.Add(JvInterpreterRecord);
end;

procedure TJvInterpreterAdapter.AddRecGet(UnitName: string; RecordType: string;
  Identifer: string; GetFunc: TJvInterpreterAdapterGetValue;
  ParamCount: Integer; ParamTypes: array of Word; ResTyp: Word);
begin
  AddRecGetEx(UnitName, RecordType, Identifer, GetFunc, ParamCount,
    ParamTypes, ResTyp, nil);
end;

procedure TJvInterpreterAdapter.AddRecGetEx(UnitName: string; RecordType: string;
  Identifer: string; GetFunc: TJvInterpreterAdapterGetValue;
  ParamCount: Integer; ParamTypes: array of Word; ResTyp: Word; Data: Pointer);
var
  RecMethod: TJvInterpreterRecMethod;
begin
  RecMethod := TJvInterpreterRecMethod.Create;
  RecMethod.JvInterpreterRecord := GetRec(RecordType) as TJvInterpreterRecord;
  RecMethod.Identifer := Identifer;
  RecMethod.Func := @GetFunc;
  RecMethod.ParamCount := ParamCount;
  RecMethod.ResTyp := ResTyp;
  RecMethod.Data := Data;
  ConvertParamTypes(ParamTypes, RecMethod.ParamTypes);
  FRecGetList.Add(RecMethod);
end;

procedure TJvInterpreterAdapter.AddRecSet(UnitName: string; RecordType: string;
  Identifer: string; SetFunc: TJvInterpreterAdapterSetValue;
  ParamCount: Integer; ParamTypes: array of Word);
begin
  AddRecSetEx(UnitName, RecordType, Identifer, SetFunc, ParamCount, ParamTypes,
    nil);
end;

procedure TJvInterpreterAdapter.AddRecSetEx(UnitName: string; RecordType: string;
  Identifer: string; SetFunc: TJvInterpreterAdapterSetValue;
  ParamCount: Integer; ParamTypes: array of Word; Data: Pointer);
var
  RecMethod: TJvInterpreterRecMethod;
begin
  RecMethod := TJvInterpreterRecMethod.Create;
  RecMethod.JvInterpreterRecord := GetRec(RecordType) as TJvInterpreterRecord;
  RecMethod.Identifer := Identifer;
  RecMethod.Func := @SetFunc;
  RecMethod.ParamCount := ParamCount;
  RecMethod.Data := Data;
  ConvertParamTypes(ParamTypes, RecMethod.ParamTypes);
  FRecSetList.Add(RecMethod);
end;

procedure TJvInterpreterAdapter.AddConst(UnitName: string; Identifer: string;
  Value: Variant);
begin
  AddConstEx(UnitName, Identifer, Value, nil);
end;

procedure TJvInterpreterAdapter.AddConstEx(UnitName: string; Identifer: string;
  Value: Variant; Data: Pointer);
var
  JvInterpreterConst: TJvInterpreterConst;
begin
  JvInterpreterConst := TJvInterpreterConst.Create;
  JvInterpreterConst.Identifer := Identifer;
  JvInterpreterConst.Value := Value;
  JvInterpreterConst.Data := Data;
  FConstList.Add(JvInterpreterConst);
  FSorted:=false;  // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddExtFun(UnitName: string; Identifer: string;
  DllInstance: HINST; DllName: string; FunName: string; FunIndex: integer;
  ParamCount: Integer; ParamTypes: array of Word; ResTyp: Word);
begin
  AddExtFunEx(UnitName, Identifer, DllInstance, DllName, FunName, FunIndex,
    ParamCount, ParamTypes, ResTyp, nil);
end;

procedure TJvInterpreterAdapter.AddExtFunEx(UnitName: string; Identifer: string;
  DllInstance: HINST; DllName: string; FunName: string; FunIndex: integer;
  ParamCount: Integer; ParamTypes: array of Word; ResTyp: Word; Data: Pointer);
var
  JvInterpreterExtFun: TJvInterpreterExtFun;
begin
  JvInterpreterExtFun := TJvInterpreterExtFun.Create;
  JvInterpreterExtFun.FunDesc.FUnitName := UnitName;
  JvInterpreterExtFun.Identifer := Identifer;
  JvInterpreterExtFun.DllInstance := DllInstance;
  JvInterpreterExtFun.DllName := DllName;
  JvInterpreterExtFun.FunName := FunName;
  JvInterpreterExtFun.FunIndex := FunIndex;
  JvInterpreterExtFun.FunDesc.FParamCount := ParamCount;
  JvInterpreterExtFun.FunDesc.FResTyp := ResTyp;
  JvInterpreterExtFun.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterExtFun.FunDesc.FParamTypes);
  FExtFunList.Add(JvInterpreterExtFun);
end;

procedure TJvInterpreterAdapter.AddSrcFun(UnitName: string; Identifer: string;
  PosBeg, PosEnd: integer; ParamCount: Integer; ParamTypes: array of Word;
  ParamNames: array of string; ResTyp: Word; Data: Pointer);
begin
  AddSrcFunEx(UnitName, Identifer, PosBeg, PosEnd, ParamCount, ParamTypes,
    ParamNames, ResTyp, nil);
end;

procedure TJvInterpreterAdapter.AddSrcFunEx(UnitName: string; Identifer: string;
  PosBeg, PosEnd: integer; ParamCount: Integer; ParamTypes: array of Word;
  ParamNames: array of string; ResTyp: Word; Data: Pointer);
var
  JvInterpreterSrcFun: TJvInterpreterSrcFun;
begin
  JvInterpreterSrcFun := TJvInterpreterSrcFun.Create;
  JvInterpreterSrcFun.FunDesc.FUnitName := UnitName;
  JvInterpreterSrcFun.FunDesc.FIdentifer := Identifer;
  JvInterpreterSrcFun.FunDesc.FPosBeg := PosBeg;
  JvInterpreterSrcFun.FunDesc.FPosEnd := PosEnd;
  JvInterpreterSrcFun.FunDesc.FParamCount := ParamCount;
  JvInterpreterSrcFun.FunDesc.FResTyp := ResTyp;
  JvInterpreterSrcFun.Identifer := Identifer;
  JvInterpreterSrcFun.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterSrcFun.FunDesc.FParamTypes);
  ConvertParamNames(ParamNames, JvInterpreterSrcFun.FunDesc.FParamNames);
  JvInterpreterSrcFun.FunDesc.FResTyp := ResTyp;
  FSrcFunList.Add(JvInterpreterSrcFun);
end;

procedure TJvInterpreterAdapter.AddHandler(UnitName: string; Identifer: string;
  EventClass: TJvInterpreterEventClass; Code: Pointer);
begin
  AddHandlerEx(UnitName, Identifer, EventClass, Code, nil);
end;

procedure TJvInterpreterAdapter.AddHandlerEx(UnitName: string; Identifer: string;
  EventClass: TJvInterpreterEventClass; Code: Pointer; Data: Pointer);
var
  JvInterpreterEventDesc: TJvInterpreterEventDesc;
begin
  JvInterpreterEventDesc := TJvInterpreterEventDesc.Create;
  JvInterpreterEventDesc.UnitName := UnitName;
  JvInterpreterEventDesc.Identifer := Identifer;
  JvInterpreterEventDesc.EventClass := EventClass;
  JvInterpreterEventDesc.Code := Code;
  JvInterpreterEventDesc.Data := Data;
  FEventHandlerList.Add(JvInterpreterEventDesc);
end;

procedure TJvInterpreterAdapter.AddEvent(UnitName: string; ClassType: TClass;
  Identifer: string);
begin
  AddEventEx(UnitName, ClassType, Identifer, nil);
end;

procedure TJvInterpreterAdapter.AddEventEx(UnitName: string; ClassType: TClass;
  Identifer: string; Data: Pointer);
var
  JvInterpreterEvent: TJvInterpreterClass;
begin
  JvInterpreterEvent := TJvInterpreterClass.Create;
  JvInterpreterEvent.UnitName := UnitName;
  JvInterpreterEvent.Identifer := Identifer;
  JvInterpreterEvent.ClassType := ClassType;
  JvInterpreterEvent.Data := Data;
  FEventList.Add(JvInterpreterEvent);
end;

procedure TJvInterpreterAdapter.AddSrcVar(UnitName: string; Identifer, Typ: string;
  VTyp: Word; const Value: Variant);
begin
  FSrcVarList.AddVar(UnitName, Identifer, Typ, VTyp, Value);
end;

procedure TJvInterpreterAdapter.AddSrcClass(JvInterpreterSrcClass: TJvInterpreterIdentifer);
begin
  FSrcClassList.Add(JvInterpreterSrcClass);
end;

function TJvInterpreterAdapter.GetSrcClass(Identifer: string): TJvInterpreterIdentifer;
begin
  Result := FSrcClassList.IndexOf('', Identifer);
end;

procedure TJvInterpreterAdapter.AddOnGet(Method: TJvInterpreterGetValue);
var
  PM: PMethod;
begin
  New(PM);
  PM^ := TMethod(Method);
  FOnGetList.Add(PM);
end;

procedure TJvInterpreterAdapter.AddOnSet(Method: TJvInterpreterSetValue);
var
  PM: PMethod;
begin
  New(PM);
  PM^ := TMethod(Method);
  FOnSetList.Add(PM);
end;

function TJvInterpreterAdapter.GetRec(RecordType: string): TObject;
var
  i: Integer;
begin
  for i := 0 to FRecList.Count - 1 do { Iterate }
  begin
    Result := FRecList[i];
    if Cmp(TJvInterpreterRecord(Result).Identifer, RecordType) then
      Exit;
  end; { for }
  //JvInterpreterErrorN(ieRecordNotDefined, -1, RecordType);
  Result := nil;
end; { GetRec }

procedure TJvInterpreterAdapter.CheckArgs(var Args: TArgs; ParamCount: Integer;
  var ParamTypes: TTypeArray);
var
  i: Integer;
begin
  if ParamCount = prArgsNoCheck then Exit;
  if Args.Count > ParamCount then
    JvInterpreterError(ieTooManyParams, -1);
  if Args.Count < ParamCount then
    JvInterpreterError(ieNotEnoughParams, -1);

  Args.HasVars := False;
  Args.Types := ParamTypes;
  for i := 0 to Args.Count - 1 do { Iterate }
    if (Args.VarNames[i] <> '') and
      ((ParamTypes[i] and varByRef) <> 0) then
    begin
      Args.HasVars := True;
      Break;
    end;
end; { CheckArgs }

procedure TJvInterpreterAdapter.CheckAction(Expression: TJvInterpreterExpression; Args: TArgs;
  Data: Pointer);
begin
  // abstract
end; { CheckAction }

function TJvInterpreterAdapter.FindFunDesc(const UnitName: string;
  const Identifer: string): TJvInterpreterFunDesc;
var
  i: Integer;
begin
  for i := FSrcFunList.Count - 1 downto 0 do { Iterate }
  begin
    Result := TJvInterpreterSrcFun(FSrcFunList.Items[i]).FunDesc;
    if Cmp(Result.Identifer, Identifer) and
       (Cmp(Result.UnitName, UnitName) or (UnitName = '')) then
      Exit;
  end; { for }
  if UnitName <> '' then
    Result := FindFunDesc('', Identifer)
  else
    Result := nil;
end; { FindFunDesc }

function TJvInterpreterAdapter.GetValue(Expression: TJvInterpreterExpression; Identifer: string;
  var Value: Variant; Args: TArgs): Boolean;

  function GetMethod: boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterMethod;
  begin
    Result := GetValueRTTI(Identifer, Value, Args);
    if Result then Exit;
    if FGetList.Find(Identifer, i) then
      for i := i to FGetList.Count - 1 do { Iterate }
      begin
        JvInterpreterMethod := TJvInterpreterMethod(FGetList[i]);
        if Assigned(JvInterpreterMethod.Func) and
          (((Args.ObjTyp = varObject) and
          (Args.Obj is JvInterpreterMethod.ClassType)) or
          ((Args.ObjTyp = varClass) and
          (TClass(Args.Obj) = JvInterpreterMethod.ClassType))) {?!} then
        begin
          Args.Identifer := Identifer;
          CheckAction(Expression, Args, JvInterpreterMethod.Data);
          CheckArgs(Args, JvInterpreterMethod.ParamCount, JvInterpreterMethod.ParamTypes);
          TJvInterpreterAdapterGetValue(JvInterpreterMethod.Func)(Value, Args);
          Result := True;
          Exit;
        end;
        if not Cmp(JvInterpreterMethod.Identifer, Identifer) then Break;
      end;
    if Cmp(Identifer, 'Free') then
    begin
      Result := True;
      Args.Obj.Free;
      Args.Obj := nil;
      Value := Null;
      Exit;
    end;
  end; {  }

  function IGetMethod: boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterMethod;
  begin
    if FIGetList.Find(Identifer, i) then
      for i := i to FIGetList.Count - 1 do { Iterate }
      begin
        JvInterpreterMethod := TJvInterpreterMethod(FIGetList[i]);
        if Assigned(JvInterpreterMethod.Func) and
          (((Args.ObjTyp = varObject) and
          (Args.Obj is JvInterpreterMethod.ClassType)) or
          ((Args.ObjTyp = varClass) and
          (TClass(Args.Obj) = JvInterpreterMethod.ClassType))) {?!} then
        begin
          Args.Identifer := Identifer;
          CheckAction(Expression, Args, JvInterpreterMethod.Data);
          CheckArgs(Args, JvInterpreterMethod.ParamCount, JvInterpreterMethod.ParamTypes);
          TJvInterpreterAdapterGetValue(JvInterpreterMethod.Func)(Value, Args);
          Result := True;
          Args.ReturnIndexed := True;
          Exit;
        end;
        if not Cmp(JvInterpreterMethod.Identifer, Identifer) then Break;
      end;
    Result := False;
  end; {  }

  { function DGetMethod is under construction }
  function DGetMethod: boolean;
  var
    JvInterpreterMethod: TJvInterpreterDMethod;
    i, j: Integer;
    Aint: integer;
    Aword : word;
    iRes: integer;
    Func: Pointer;
    REAX, REDX, RECX: Integer;
  begin
    Result := False;
    iRes := 0; { satisfy compiler }
    for i := 0 to FDGetList.Count - 1 do    { Iterate }
    begin
      JvInterpreterMethod := TJvInterpreterDMethod(FDGetList[i]);
      Func := JvInterpreterMethod.Func;
      if Assigned(JvInterpreterMethod.Func) and
         (((Args.ObjTyp = varObject) and
          (Args.Obj is JvInterpreterMethod.ClassType)) { or
          ((Args.ObjTyp = varClass) and
          (TClass(Args.Obj) = JvInterpreterMethod.ClassType))})  and
         Cmp(JvInterpreterMethod.Identifer, Identifer) then
      begin
        Args.Identifer := Identifer;
        CheckAction(Expression, Args, JvInterpreterMethod.Data);
        CheckArgs(Args, JvInterpreterMethod.ParamCount, JvInterpreterMethod.ParamTypes);
        if ccFastCall in JvInterpreterMethod.CallConvention then
        begin
         { !!! Delphi fast-call !!! }

         { push parameters to stack }
          for j := 2 to JvInterpreterMethod.ParamCount - 1 do
            case JvInterpreterMethod.ParamTypes[j] of
              varInteger, varObject, varPointer, varBoolean{?}:
                begin Aint := Args.Values[j]; asm push Aint end; end;
              varSmallInt:
                begin Aword := Word(Args.Values[j]); asm push Aword end; end;
              else
                JvInterpreterErrorN(ieDirectInvalidArgument, -1, Identifer);
            end;
          REAX := Integer(Args.Obj);
          if JvInterpreterMethod.ParamCount > 0 then
            case JvInterpreterMethod.ParamTypes[0] of
              varInteger, varObject, varPointer, varBoolean, varSmallInt,
              varString:
                REDX := TVarData(Args.Values[0]).vInteger;
              else
                JvInterpreterErrorN(ieDirectInvalidArgument, -1, Identifer);
            end;
          if JvInterpreterMethod.ParamCount > 1 then
            case JvInterpreterMethod.ParamTypes[1] of
              varInteger, varObject, varPointer, varBoolean, varSmallInt,
              varString:
                RECX := TVarData(Args.Values[1]).vInteger
              else
                JvInterpreterErrorN(ieDirectInvalidArgument, -1, Identifer);
            end;
          case JvInterpreterMethod.ResTyp of
            varSmallInt, varInteger, varBoolean, varEmpty, varObject,
            varPointer:
              asm
                mov      EAX, REAX
                mov      EDX, REDX
                mov      ECX, RECX
                call     Func
                mov      iRes, EAX
              end;
            else
              JvInterpreterErrorN(ieDirectInvalidResult, -1, Identifer);
          end;
         { clear result }
          case JvInterpreterMethod.ResTyp of
            varInteger, varObject:
              Value := iRes;
            varSmallInt:
              Value := iRes and $0000FFFF;
            varBoolean:
              begin
                Value := iRes and $000000FF;
                TVarData(Value).VType := varBoolean;
              end;
            varEmpty:
              Value := Null;
          end;
        end
        else
          JvInterpreterErrorN(ieDirectInvalidConvention, -1, Identifer);
        Result := True;
        Exit;
      end;
    end;
  end;    {  }

  function GetRecord: Boolean;
  var
    i: Integer;
    JvInterpreterRecord: TJvInterpreterRecord;
    Rec: PChar;
    JvInterpreterRecMethod: TJvInterpreterRecMethod;
  begin
    Result := False;
    JvInterpreterRecord := (Args.Obj as TJvInterpreterRecHolder).JvInterpreterRecord;
    for i := 0 to JvInterpreterRecord.FieldCount - 1 do { Iterate }
      if Cmp(JvInterpreterRecord.Fields[i].Identifer, Identifer) then
      begin
        Rec := P2R(Args.Obj);
        with JvInterpreterRecord.Fields[i] do
          case Typ of { }
            varInteger:
              Value := PInteger(Rec + Offset)^;
            varSmallInt:
              Value := SmallInt(PWord(Rec + Offset)^);
            varBoolean:
              Value := PBool(Rec + Offset)^;
            varString:
              Value := PString(Rec + Offset)^;
          end; { case }
        Result := True;
        Exit;
      end; { for }
    for i := 0 to FRecGetList.Count - 1 do { Iterate }
    begin
      JvInterpreterRecMethod := TJvInterpreterRecMethod(FRecGetList[i]);
      if (JvInterpreterRecMethod.JvInterpreterRecord = JvInterpreterRecord) and
        Cmp(JvInterpreterRecMethod.Identifer, Identifer) then
      begin
        Args.Identifer := Identifer;
        CheckArgs(Args, JvInterpreterRecMethod.ParamCount, JvInterpreterRecMethod.ParamTypes);
        TJvInterpreterAdapterGetValue(JvInterpreterRecMethod.Func)(Value, Args);
        Result := True;
        Exit;
      end;
    end
  end;

  function GetConst: boolean;
  var
    i: Integer;
    JvInterpreterConst: TJvInterpreterConst;
  begin
    if Cmp(Identifer, 'nil') then
    begin
      Value := P2V(nil);
      Result := True;
      Exit;
    end;
    if Cmp(Identifer, 'Null') then
    begin
      Value := Null;
      Result := True;
      Exit;
    end;
    Result := FConstList.Find(Identifer, i);
    if Result then
    begin
      JvInterpreterConst := TJvInterpreterConst(FConstList[i]);
      CheckAction(Expression, Args, JvInterpreterConst.Data);
      Value := JvInterpreterConst.Value;
    end;
  end; {  }

  function GetClass: boolean;
  var
    i: Integer;
    JvInterpreterClass: TJvInterpreterClass;
  begin
    Result := FClassList.Find(Identifer, i);
    if Result then
    begin
      JvInterpreterClass := TJvInterpreterClass(FClassList[i]);
      if Args.Count = 0 then
        Value := C2V(JvInterpreterClass.ClassType)
      else
        if Args.Count = 1 then
       { typecasting }
        begin
          CheckAction(Expression, Args, JvInterpreterClass.Data);
          Value := Args.Values[0];
          if TVarData(Value).VType <> varClass then
            TVarData(Value).VType := varObject;
        end
        else
          JvInterpreterError(ieTooManyParams, -1);
    end;
  end; {  }

  function GetFun: boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterMethod;
  begin
    Result := FFunList.Find(Identifer, i);
    if Result then
    begin
      JvInterpreterMethod := TJvInterpreterMethod(FFunList[i]);
      if Cmp(JvInterpreterMethod.Identifer, Identifer) then
      begin
        Args.Identifer := Identifer;
        CheckAction(Expression, Args, JvInterpreterMethod.Data);
        CheckArgs(Args, JvInterpreterMethod.ParamCount, JvInterpreterMethod.ParamTypes);
        TJvInterpreterAdapterGetValue(JvInterpreterMethod.Func)(Value, Args);
      end;
    end;
  end; {  }

  function GetExtFun: boolean;
  var
    i: Integer;
    JvInterpreterExtFun: TJvInterpreterExtFun;
  begin
    for i := 0 to FExtFunList.Count - 1 do { Iterate }
    begin
      JvInterpreterExtFun := TJvInterpreterExtFun(FExtFunList[i]);
      if Cmp(JvInterpreterExtFun.Identifer, Identifer) then
      begin
        Args.Identifer := Identifer;
        CheckAction(Expression, Args, JvInterpreterExtFun.Data);
        CheckArgs(Args, JvInterpreterExtFun.FunDesc.ParamCount,
          JvInterpreterExtFun.FunDesc.FParamTypes);
        Value := JvInterpreterExtFun.CallDll(Args);
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end; {  }

  function GetSrcVar: boolean;
  begin
    Result := FSrcVarList.GetValue(Identifer, Value, Args);
  end; {  }

  function GetSrcUnit: boolean;
  var
    i: Integer;
    JvInterpreterSrcUnit: TJvInterpreterSrcUnit;
    FParams: TTypeArray;
  begin
    for i := 0 to FSrcUnitList.Count - 1 do { Iterate }
    begin
      JvInterpreterSrcUnit := TJvInterpreterSrcUnit(FSrcUnitList[i]);
      if Cmp(JvInterpreterSrcUnit.Identifer, Identifer) then
      begin
        CheckArgs(Args, 0, FParams);
        Value := O2V(JvInterpreterSrcUnit);
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end; {  }

 {$IFDEF JvInterpreter_OLEAUTO}
  function GetOleAutoFun: boolean;
  var
    FParams: TTypeArray;
  begin
    Result := False;
    if Cmp(Identifer, 'CreateOleObject') or
      Cmp(Identifer, 'GetActiveOleObject') or
      Cmp(Identifer, 'GetOleObject') then
    begin
      FParams[0] := varString;
      CheckArgs(Args, 1, FParams);
      if Cmp(Identifer, 'CreateOleObject') then
        Value := CreateOleObject(Args.Values[0])
      else if Cmp(Identifer, 'CreateOleObject') then
        Value := GetActiveOleObject(Args.Values[0])
      else { GetOleObject }
      begin
        try
          Value := GetActiveOleObject(Args.Values[0])
        except
          on E: EOleError do
            Value := CreateOleObject(Args.Values[0])
        end;
      end;
      Result := True;
      Exit;
    end;
  end; {  }
{$ENDIF JvInterpreter_OLEAUTO}

  function TypeCast: Boolean;
  var
    VT: Word;
  begin
    VT := TypeName2VarTyp(Identifer);
    Result := VT <> varEmpty;
    if Result then
    begin
      Value := Args.Values[0];
      TVarData(Value).VType := VT;
    end;
  end;

var
  i: Integer;
begin
  Result := True;
  if not FSorted then Sort;

  if Args.Indexed then
  begin
    if (Args.Obj <> nil) and (Args.ObjTyp in [varObject, varClass]) then
      if IGetMethod then Exit;
  end
  else
  begin
    if Args.Obj <> nil then
    begin
     { methods }
      if Args.ObjTyp in [varObject, varClass] then
        if GetMethod or DGetMethod then Exit else
      else if Args.ObjTyp = varRecord then
        if (Args.Obj is TJvInterpreterRecHolder) and GetRecord then Exit else
      else if Args.ObjTyp = varDispatch then
     { Ole automation call }
      begin
      {$IFDEF JvInterpreter_OLEAUTO}
        Result := DispatchCall(Identifer, Value, Args, True);
        if Result then Exit;
      {$ELSE}
        NotImplemented('Ole automation call');
      {$ENDIF JvInterpreter_OLEAUTO}
      end;
    end
    else {if Args.Obj = nil then }
    begin
     { classes }
      if GetClass then Exit;
     { constants }
      if GetConst then Exit;
     { classless functions and procedures }
      if GetFun then Exit;
     { external functions }
      if GetExtFun then Exit;
     {$IFDEF JvInterpreter_OLEAUTO}
      if GetOleAutoFun then Exit;
     {$ENDIF JvInterpreter_OLEAUTO}
      if TypeCast then Exit;
    end;
  end;

 { source variables and constants }
  if GetSrcVar then Exit;

  if not ((Args.Obj <> nil) and (Args.ObjTyp in [varObject, varClass])) then // ivan_ra
  if GetSrcUnit then Exit;

  for i := 0 to FOnGetList.Count - 1 do { Iterate }
  begin
    TJvInterpreterGetValue(FOnGetList[i]^)(Self, Identifer, Value, Args, Result);
    if Result then Exit;
  end;
  Result := False;
end;

function TJvInterpreterAdapter.SetValue(Expression: TJvInterpreterExpression; Identifer: string;
  const Value: Variant; Args: TArgs): Boolean;

  function SetMethod: boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterMethod;
  begin
    Result := SetValueRTTI(Identifer, Value, Args);
    if Result then Exit;
    for i := 0 to FSetList.Count - 1 do { Iterate }
    begin
      JvInterpreterMethod := TJvInterpreterMethod(FSetList[i]);
      if Assigned(JvInterpreterMethod.Func) and
        (Args.Obj is JvInterpreterMethod.ClassType) and
        Cmp(JvInterpreterMethod.Identifer, Identifer) then
      begin
        Args.Identifer := Identifer;
        CheckAction(Expression, Args, JvInterpreterMethod.Data);
        CheckArgs(Args, JvInterpreterMethod.ParamCount, JvInterpreterMethod.ParamTypes);
        TJvInterpreterAdapterSetValue(JvInterpreterMethod.Func)(Value, Args);
        Result := True;
        Exit;
      end;
    end;
  end; {  }

  function ISetMethod: boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterMethod;
  begin
    Result := False;
    if FISetList.Find(Identifer, i) then
      for i := i to FISetList.Count - 1 do { Iterate }
      begin
        JvInterpreterMethod := TJvInterpreterMethod(FISetList[i]);
        if Assigned(JvInterpreterMethod.Func) and
          (Args.Obj is JvInterpreterMethod.ClassType) and
          Cmp(JvInterpreterMethod.Identifer, Identifer) then
        begin
          Args.Identifer := Identifer;
          CheckAction(Expression, Args, JvInterpreterMethod.Data);
          CheckArgs(Args, JvInterpreterMethod.ParamCount, JvInterpreterMethod.ParamTypes);
          TJvInterpreterAdapterSetValue(JvInterpreterMethod.Func)(Value, Args);
          Result := True;
          Args.ReturnIndexed := True;
          Exit;
        end;
        if not Cmp(JvInterpreterMethod.Identifer, Identifer) then Break;
      end;
  end; {  }

  function SetRecord: boolean;
  var
    i: Integer;
    JvInterpreterRecord: TJvInterpreterRecord;
    JvInterpreterRecMethod: TJvInterpreterRecMethod;
    Rec: PChar;
  begin
    Result := False;
    JvInterpreterRecord := (Args.Obj as TJvInterpreterRecHolder).JvInterpreterRecord;
    for i := 0 to JvInterpreterRecord.FieldCount - 1 do { Iterate }
      if Cmp(JvInterpreterRecord.Fields[i].Identifer, Identifer) then
      begin
        Rec := P2R(Args.Obj);
        with JvInterpreterRecord.Fields[i] do
          case Typ of { }
            varInteger:
              PInteger(Rec + Offset)^ := Value;
            varSmallInt:
              PWord(Rec + Offset)^ := Word(Value);
            varBoolean:
              PBool(Rec + Offset)^ := Value;
          end; { case }
        Result := True;
        Exit;
      end; { for }
    for i := 0 to FRecSetList.Count - 1 do { Iterate }
    begin
      JvInterpreterRecMethod := TJvInterpreterRecMethod(FRecSetList[i]);
      if (JvInterpreterRecMethod.JvInterpreterRecord = JvInterpreterRecord) and
        Cmp(JvInterpreterRecMethod.Identifer, Identifer) then
      begin
        Args.Identifer := Identifer;
        CheckArgs(Args, JvInterpreterRecMethod.ParamCount, JvInterpreterRecMethod.ParamTypes);
        TJvInterpreterAdapterSetValue(JvInterpreterRecMethod.Func)(Value, Args);
        Result := True;
        Exit;
      end;
    end
  end; {  }

  function SetSrcVar: boolean;
  begin
    Result := FSrcVarList.SetValue(Identifer, Value, Args);
  end; {  }

var
  i: Integer;
{$IFDEF JvInterpreter_OLEAUTO}
  V: Variant;
{$ENDIF JvInterpreter_OLEAUTO}
begin
  Result := True;
  if not FSorted then Sort;

  if Args.Indexed then
  begin
    if (Args.Obj <> nil) and (Args.ObjTyp in [varObject, varClass]) then
      if ISetMethod then Exit;
  end
  else
  begin
    if Args.Obj <> nil then
    begin
     { methods }
      if Args.ObjTyp in [varObject, varClass] then
        if SetMethod then Exit else
      else if Args.ObjTyp = varRecord then
        if (Args.Obj is TJvInterpreterRecHolder) and SetRecord then Exit else
      else if Args.ObjTyp = varDispatch then
     { Ole automation call }
      begin
       {$IFDEF JvInterpreter_OLEAUTO}
        V := Value;
        Result := DispatchCall(Identifer, V, Args, False);
        if Result then Exit;
       {$ELSE}
        NotImplemented('Ole automation call');
       {$ENDIF JvInterpreter_OLEAUTO}
      end;
    end;
  end;

  { source variables and constants }
  if SetSrcVar then Exit;

  for i := 0 to FOnSetList.Count - 1 do { Iterate }
  begin
    TJvInterpreterSetValue(FOnSetList[i]^)(Self, Identifer, Value, Args, Result);
    if Result then Exit;
  end;
  Result := False;
end;

function TJvInterpreterAdapter.GetElement(Expression: TJvInterpreterExpression;
  const Variable: Variant; var Value: Variant; var Args: TArgs): Boolean;

  function GetID: boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterMethod;
    Obj: TObject;
  begin
    Obj := V2O(Variable);
    for i := 0 to FIDGetList.Count - 1 do    { Iterate }
    begin
      JvInterpreterMethod := TJvInterpreterMethod(FIDGetList[i]);
      if Obj is JvInterpreterMethod.ClassType then
      begin
        Args.Obj := Obj;
        CheckAction(Expression, Args, JvInterpreterMethod.Data);
        CheckArgs(Args, JvInterpreterMethod.ParamCount, JvInterpreterMethod.ParamTypes);
        TJvInterpreterAdapterGetValue(JvInterpreterMethod.Func)(Value, Args);
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;

begin
  Result := True;
  { defaul indexed properties }
  if TVarData(Variable).VType = varObject then
  begin
    if GetID then Exit;
    Result := False;
  end
  else
    Result := False;
end;

function TJvInterpreterAdapter.SetElement(Expression: TJvInterpreterExpression;
  var Variable: Variant; const Value: Variant; var Args: TArgs): Boolean;

  function SetID: boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterMethod;
    Obj: TObject;
  begin
    Obj := V2O(Variable);
    for i := 0 to FIDSetList.Count - 1 do    { Iterate }
    begin
      JvInterpreterMethod := TJvInterpreterMethod(FIDSetList[i]);
      if Obj is JvInterpreterMethod.ClassType then
      begin
        Args.Obj := Obj;
        CheckAction(Expression, Args, JvInterpreterMethod.Data);
        CheckArgs(Args, JvInterpreterMethod.ParamCount, JvInterpreterMethod.ParamTypes);
        TJvInterpreterAdapterSetValue(JvInterpreterMethod.Func)(Value, Args);
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;

begin
  Result := True;
  { defaul indexed properties }
  if TVarData(Variable).VType = varObject then
  begin
    if SetID then Exit;
    Result := False;
  end
  else
    Result := False;
end;

function TJvInterpreterAdapter.SetRecord(var Value: Variant): Boolean;
var
  RecHolder: TJvInterpreterRecHolder;
begin
  if TVarData(Value).VType = varRecord then
  begin
    RecHolder := TJvInterpreterRecHolder(TVarData(Value).vPointer);
    RecHolder.JvInterpreterRecord := TJvInterpreterRecord(GetRec(RecHolder.RecordType));
    Result := Assigned(RecHolder.JvInterpreterRecord);
  end
  else
    Result := False;
end;

function TJvInterpreterAdapter.NewRecord(const RecordType: string;
  var Value: Variant): Boolean;
var
  i, j: Integer;
  JvInterpreterRecord: TJvInterpreterRecord;
  Rec: PChar;
begin
  for i := 0 to FRecList.Count - 1 do { Iterate }
  begin
    JvInterpreterRecord := TJvInterpreterRecord(FRecList[i]);
    if Cmp(JvInterpreterRecord.Identifer, RecordType) then
    begin
      if Assigned(JvInterpreterRecord.CreateFunc) then
        JvInterpreterRecord.CreateFunc(Pointer(Rec))
      else
      begin
        GetMem(Rec, JvInterpreterRecord.RecordSize);
        for j := 0 to JvInterpreterRecord.FieldCount - 1 do { Iterate }
          if JvInterpreterRecord.Fields[j].Typ = varString then
            PString(PString(Rec + JvInterpreterRecord.Fields[j].Offset)^) := @EmptyStr;
      end;
      JvInterpreterVarCopy(Value, R2V(RecordType, Rec));
      Result := SetRecord(Value);
      Exit;
    end;
  end; { for }
  Result := False;
end;

{$IFDEF JvInterpreter_OLEAUTO}

function TJvInterpreterAdapter.DispatchCall(Identifer: string; var Value: Variant;
  Args: TArgs; Get: Boolean): Boolean; stdcall;
var
  CallDesc: TCallDesc;
  ParamTypes: array[0..MaxDispArgs * 4 - 1] of byte;
  ParamsPtr: Pointer absolute ParamTypes;
  Ptr: Integer;
  TypePtr: Integer;

  procedure AddParam(Param: Variant);

    procedure AddParam1(Typ: byte; ParamSize: Integer; var Param);
    begin
     { CallDesc.ArgTypes[Ptr] := Typ;
      Move(Param, ParamTypes[Ptr], ParamSize);
      inc(Ptr, ParamSize); }
      CallDesc.ArgTypes[TypePtr] := Typ;
      Move(Param, ParamTypes[Ptr], ParamSize);
      inc(Ptr, ParamSize);
      inc(TypePtr); 
    end;

  var
    Int: Integer;
    Wrd: WordBool;
    Poin: Pointer;
    //TempDisp : IDispatch;
  begin
    case TVarData(Param).VType of
      varInteger:
        begin
          Int := Param;
          AddParam1(varInteger, sizeof(Int), Int);
        end;
      varString:
        begin
          Poin := V2P(Param);
          AddParam1(varStrArg, sizeof(Poin), Poin);
        end;
      varBoolean:
        begin
          Wrd := WordBool(Param);
          AddParam1(varBoolean, sizeof(Wrd), Wrd);
        end;
     { varDispatch:
        begin
          TempDisp := VarToInterface(Param.IFace);
          AddParam1(varDispatch, sizeof(TempDisp), TempDisp);
        end; }

    end;
  end; { AddParam1 }

var
  PVRes: PVariant;
  Names: string;
  i: Integer;
begin
  Result := True;
 {Call mathod through Ole Automation}
  with CallDesc do
  begin
    if Get then
      CallType := DISPATCH_METHOD or DISPATCH_PROPERTYGET
    else
      CallType := DISPATCH_PROPERTYPUT;
    ArgCount := Args.Count;
    NamedArgCount := 0; { named args not supported by JvInterpreter }
  end;
  Names := Identifer + #00;
  Ptr := 0;
  TypePtr := 0;
  if not Get then
  begin
    AddParam(Value);
    inc(CallDesc.ArgCount);
  end;
  for i := 0 to Args.Count - 1 do
    AddParam(Args.Values[i]);
  Value := Null;
 { When calling procedures(without result) PVRes must be nil }
  if Args.HasResult and Get then
    PVRes := @Value
  else
    PVRes := nil;
  try
   { call }
    VarDispInvoke(PVRes, Args.Obj, PChar(Names), @CallDesc, ParamsPtr);
  except
    on E: EOleError do
      JvInterpreterErrorN2(ieOleAuto, -1, Identifer, E.Message);
  end;
  if Get and (TVarData(Value).VType = varOleStr) then
    Value := VarAsType(Value, varString);
end;
{$ENDIF JvInterpreter_OLEAUTO}

function TJvInterpreterAdapter.GetValueRTTI(Identifer: string; var Value: Variant;
  Args: TArgs): Boolean;
var
  TypeInf: PTypeInfo;
  PropInf: PPropInfo;
  PropTyp: TypInfo.TTypeKind;
begin
  Result := False;
  if (Args.ObjTyp <> varObject) or (Args.Obj = nil) then Exit;
  TypeInf := Args.Obj.ClassInfo;
  if TypeInf = nil then Exit;
  PropInf := GetPropInfo(TypeInf, Identifer);
  if PropInf = nil then Exit;
  PropTyp := PropInf.PropType^.Kind;
  case PropTyp of
    tkInteger, tkEnumeration:
      Value := GetOrdProp(Args.Obj, PropInf);
    tkChar, tkWChar:
      Value := Char(GetOrdProp(Args.Obj, PropInf));
    tkFloat:
      Value := GetFloatProp(Args.Obj, PropInf);
    tkString, tkLString{$IFDEF COMPILER3_UP}, tkWString{$ENDIF COMPILER3_UP}:
      Value := GetStrProp(Args.Obj, PropInf);
    tkClass:
      Value := O2V(TObject(GetOrdProp(Args.Obj, PropInf)));
    tkSet:
      Value := S2V(GetOrdProp(Args.Obj, PropInf));
  else
    Exit;
  end;
  if PropInf^.PropType^.Name = 'Boolean' then
    TVarData(Value).VType := varBoolean;
  Result := True;
end;

function TJvInterpreterAdapter.SetValueRTTI(Identifer: string; const Value: Variant;
  Args: TArgs): Boolean;
var
  TypeInf: PTypeInfo;
  PropInf: PPropInfo;
  PropTyp: TypInfo.TTypeKind;
  Obj: TObject;
begin
  Result := False;
  if (Args.ObjTyp <> varObject) or (Args.Obj = nil) then Exit;
  Obj := Args.Obj;
  TypeInf := Obj.ClassInfo;
  if TypeInf = nil then Exit;
  PropInf := GetPropInfo(TypeInf, Identifer);
  if PropInf = nil then Exit;
  PropTyp := PropInf.PropType^.Kind;
  case PropTyp of
    tkInteger, tkEnumeration:
      SetOrdProp(Args.Obj, PropInf, Var2Type(Value, varInteger));
    tkChar, tkWChar:
      SetOrdProp(Args.Obj, PropInf, integer(string(Value)[1]));
    tkFloat:
      SetFloatProp(Args.Obj, PropInf, Value);
    tkString, tkLString{$IFDEF COMPILER3_UP}, tkWString{$ENDIF COMPILER3_UP}:
      SetStrProp(Args.Obj, PropInf, VarToStr(Value));
    tkClass:
      SetOrdProp(Args.Obj, PropInf, integer(V2O(Value)));
    tkSet:
      SetOrdProp(Args.Obj, PropInf, V2S(Value));
  else
    Exit;
  end;
  Result := True;
end;

procedure TJvInterpreterAdapter.CurUnitChanged(NewUnitName: string; var Source: string);
var
  i: Integer;
  JvInterpreterUnitSource: TJvInterpreterSrcUnit;
begin
  for i := 0 to FSrcUnitList.Count - 1 do { Iterate }
  begin
    JvInterpreterUnitSource := TJvInterpreterSrcUnit(FSrcUnitList.Items[i]);
    if Cmp(TJvInterpreterSrcUnit(JvInterpreterUnitSource).Identifer, NewUnitName) then
    begin
      Source := TJvInterpreterSrcUnit(JvInterpreterUnitSource).Source;
      Exit;
    end;
  end; { for }
  Source := '';
end;

function TJvInterpreterAdapter.UnitExists(const Identifer: string): Boolean;
var
  JvInterpreterIdentifer: TJvInterpreterIdentifer;
  i: Integer;
begin
  Result := True;
  for i := 0 to FSrcUnitList.Count - 1 do { Iterate }
  begin
    JvInterpreterIdentifer := TJvInterpreterIdentifer(FSrcUnitList.Items[i]);
    if Cmp(JvInterpreterIdentifer.Identifer, Identifer) then
      Exit;
  end; { for }
  for i := 0 to FExtUnitList.Count - 1 do { Iterate }
  begin
    JvInterpreterIdentifer := TJvInterpreterIdentifer(FExtUnitList.Items[i]);
    if Cmp(JvInterpreterIdentifer.Identifer, Identifer) then
      Exit;
  end; { for }
  Result := False;
end;

function TJvInterpreterAdapter.NewEvent(const UnitName: string; const FunName,
  EventType: string; AOwner: TJvInterpreterExpression; AObject: TObject): TSimpleEvent;
var
  Event: TJvInterpreterEvent;
  i: Integer;
  JvInterpreterEventDesc: TJvInterpreterEventDesc;
begin
  for i := 0 to FEventHandlerList.Count - 1 do { Iterate }
  begin
    JvInterpreterEventDesc := TJvInterpreterEventDesc(FEventHandlerList.Items[i]);
    if Cmp(JvInterpreterEventDesc.Identifer, EventType) then
    begin
      Event := JvInterpreterEventDesc.EventClass.Create(AOwner, AObject, UnitName, FunName);
      TMethod(Result).Code := JvInterpreterEventDesc.Code;
      TMethod(Result).Data := Event;
      Exit;
    end;
  end; { for }
  Result := nil; { satisfy compiler }
end; { NewEvent }

function TJvInterpreterAdapter.IsEvent(Obj: TObject; const Identifer: string)
  : Boolean;
var
  JvInterpreterClass: TJvInterpreterClass;
  i: Integer;
begin
  for i := 0 to FEventList.Count - 1 do { Iterate }
  begin
    JvInterpreterClass := TJvInterpreterClass(FEventList[i]);
    if (Obj is JvInterpreterClass.ClassType) and
      Cmp(JvInterpreterClass.Identifer, Identifer) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TJvInterpreterAdapter.Sort;
begin
  FConstList.Sort;
  FClassList.Sort;
  FFunList.Sort;
  FGetList.Sort;
  FSetList.Sort;
  FIGetList.Sort;
  FISetList.Sort;
  FSorted := True;
end;


{ ************************* TArgs ************************* }

procedure TArgs.Clear;
begin
  Count := 0;
  Obj := nil;
  ObjTyp := 0;
  HasVars := False;
  Indexed := False;
  ReturnIndexed := False;
end;

destructor TArgs.Destroy;
begin
  if OA <> nil then Dispose(OA);
  if OAV <> nil then Dispose(OAV);
  inherited Destroy;
end; { Destroy }

procedure TArgs.OpenArray(const index: Integer);
begin
  if OA = nil then New(OA);
  if OAV = nil then New(OAV);
  V2OA(Values[index], OA^, OAV^, OAS);
end;

procedure TArgs.Delete(const Index: Integer);
var
  i: Integer;
begin
  for i := Index to Count - 2 do
  begin
    Types[i] := Types[i + 1];
    Values[i] := Values[i + 1];
    Names[i] := Names[i + 1];
  end;
  dec(Count);
end;

{ ************************* TJvInterpreterExpression ************************* }

constructor TJvInterpreterExpression.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parser := TJvInterpreterParser.Create;
  FPStream := TStringStream .Create('');
  FArgs := TArgs.Create;
  FAdapter := CreateAdapter;
  FSharedAdapter := GlobalJvInterpreterAdapter;
  FLastError := EJvInterpreterError.Create(-1, -1, '', '');
  AllowAssignment := True;
end; { Create }

destructor TJvInterpreterExpression.Destroy;
begin
  JvInterpreterVarFree(FVResult);
  FAdapter.Free;
  FArgs.Free;
  FPStream.Free;
  Parser.Free;
  FLastError.Free;
  inherited Destroy;
end; { Destroy }

procedure TJvInterpreterExpression.UpdateExceptionPos(E: Exception; const UnitName: string);

  procedure NONAME1(E: EJvInterpreterError);
  begin
    if not E.FExceptionPos then
    begin
      if E.FErrPos = -1 then
        E.FErrPos := CurPos;
      if E.FErrUnitName = '' then
        E.FErrUnitName := UnitName;
      if E.FErrUnitName <> '' then
      begin
        E.FErrLine := GetLineByPos(Parser.Source, E.FErrPos) + BaseErrLine + 1
           { first line has number 1 };
        E.Message := Format(LoadStr2(ieErrorPos),
          [E.FErrUnitName, E.FErrLine, E.FMessage1]);
        E.FExceptionPos := True;
      end;
    end;
  end;

begin
  if E is EJvInterpreterError then
  begin
    NONAME1(E as EJvInterpreterError);
    FLastError.Assign(E as EJvInterpreterError);
  end
  else if not FLastError.FExceptionPos then
  begin
    FLastError.FErrCode := ieExternal;
    FLastError.Message := E.Message;
    FLastError.FMessage1 := E.Message;
    NONAME1(FLastError);
  end;
end; { UpdateExceptionPos }

procedure TJvInterpreterExpression.Init;
begin
  FVResult := Null;
  ExpStackPtr := -1;
 // Parse;
  Parser.Init;
  FBacked := False;
  Args := FArgs;
  FAdapter.ClearNonSource;
  FLastError.Clear;
end; { Init }

function TJvInterpreterExpression.GetSource: string;
begin
  Result := Parser.Source;
end; { GetSource }

procedure TJvInterpreterExpression.SetSource(Value: string);
begin
  Parser.Source := Value;
  SourceChanged;
end; { SetSource }

procedure TJvInterpreterExpression.SourceChanged;
begin
end; { SourceChanged }

procedure TJvInterpreterExpression.SetAdapter(Adapter: TJvInterpreterAdapter);
begin
  FAdapter := Adapter;
end; { SetAdapter }

procedure TJvInterpreterExpression.SetCurPos(Value: Integer);
begin
  if FParsed then
    FPStream.Position := Value
  else
    Parser.Pos := Value;
  FBacked := False;
end; { SetCurPos }

function TJvInterpreterExpression.GetCurPos: Integer;
begin
  if FParsed then
    Result := FPStream.Position
  else
    Result := Parser.Pos;
end; { GetCurPos }

procedure TJvInterpreterExpression.ErrorExpected(Exp: string);
begin
  if TokenStr <> '' then
    JvInterpreterErrorN2(ieExpected, PosBeg, Exp, '''' + TokenStr + '''')
  else
    JvInterpreterErrorN2(ieExpected, PosBeg, Exp, LoadStr2(irEndOfFile));
end; { ErrorExpected }

procedure TJvInterpreterExpression.ErrorNotImplemented(Message: string);
begin
  JvInterpreterErrorN(ieInternal, PosBeg, Message + ' not implemented');
end; { ErrorNotImplemented }

function TJvInterpreterExpression.PosBeg: Integer;
begin
  Result := CurPos - Length(TokenStr);
end; { PosBeg }

function TJvInterpreterExpression.PosEnd: Integer;
begin
  Result := CurPos;
end; { PosEnd }

function TJvInterpreterExpression.GetTokenStr: string;
begin
  if FParsed and (TTyp <> ttUnknown) then
    Result := TypToken(TTyp)
  else
    Result := TokenStr1;
end; { GetTokenStr }

procedure TJvInterpreterExpression.Parse;
begin
{$IFNDEF COMPILER2}
  FPStream.Size := 0;
{$ELSE}
  (FPStream as TJvStringStream ).SetSize(0);
{$ENDIF}
  FPStream.Position := 0;
  Parser.Init;
  repeat
    ParseToken;
    WriteToken;
  until TTyp1 = ttEmpty;
  FParsed := True;
  FPStream.Position := 0;
end; { Parse }

procedure TJvInterpreterExpression.WriteToken;
begin
  WordSaveToStream(FPStream, Word(TTyp1));
  case TTyp1 of
    ttInteger:
      IntSaveToStream(FPStream, Token1);
    ttString:
      StringSaveToStream(FPStream, Token1);
    ttTrue, ttFalse:
      BoolSaveToStream(FPStream, Token1);
    ttDouble:
      ExtendedSaveToStream(FPStream, Token1);
    ttIdentifer:
      StringSaveToStream(FPStream, Token1);
    ttUnknown:
      StringSaveToStream(FPStream, TokenStr1);
  end;
end; { WriteToken }

procedure TJvInterpreterExpression.ReadToken;
begin
  TTyp1 := SmallInt(WordLoadFromStream(FPStream));
  case TTyp1 of
    ttInteger:
      Token1 := IntLoadFromStream(FPStream);
    ttString:
      Token1 := StringLoadFromStream(FPStream);
    ttTrue, ttFalse:
      Token1 := BoolLoadFromStream(FPStream);
    ttDouble:
      Token1 := ExtendedLoadFromStream(FPStream);
    ttIdentifer:
      Token1 := StringLoadFromStream(FPStream);
    ttUnknown:
      TokenStr1 := StringLoadFromStream(FPStream);
  end;
end; { ReadParsed }

procedure TJvInterpreterExpression.NextToken;
begin
  if FBacked then
    FBacked := False
  else
  begin
    PrevTTyp := TTyp1;
    if FParsed then
      ReadToken
    else
      ParseToken;
  end;
end; { NextToken }

procedure TJvInterpreterExpression.ParseToken;
var
  OldDecimalSeparator: char;
  Dob: Extended;
  Int: Integer;
  Stub: Integer;
begin
  TokenStr1 := Parser.Token;
  TTyp1 := TokenTyp(TokenStr1);
  case TTyp of { }
    ttInteger:
      begin
        Val(TokenStr1, Int, Stub);
        Token1 := Int;
      end;
    ttDouble:
      begin
        OldDecimalSeparator := DecimalSeparator;
        DecimalSeparator := '.';
        if not TextToFloat(PChar(TokenStr1), Dob, fvExtended) then
        begin
          DecimalSeparator := OldDecimalSeparator;
          JvInterpreterError(ieInternal, -1);
        end
        else
          DecimalSeparator := OldDecimalSeparator;
        Token1 := Dob;
      end;
    ttString:
      Token1 := Copy(TokenStr, 2, Length(TokenStr1) - 2);
    ttFalse:
      Token1 := False;
    ttTrue:
      Token1 := True;
    ttIdentifer:
      Token1 := TokenStr1;
    {-----olej-----}
    ttArray:
      Token1 := TokenStr1;
    {-----olej-----}
  end; { case }
end; { ParseToken }

procedure TJvInterpreterExpression.Back;
begin
//  JvInterpreterError(ieInternal, -2);
  if FBacked then
    JvInterpreterError(ieInternal, -1);
  FBacked := True;
end; { Back }

procedure TJvInterpreterExpression.SafeBack;
begin
  if not FBacked then
    Back;
end; { SafeBack }

function TJvInterpreterExpression.CreateAdapter: TJvInterpreterAdapter;
begin
  Result := TJvInterpreterAdapter.Create(Self);
end; { CreateAdapter }

function TJvInterpreterExpression.Expression1: Variant;

  procedure PushExp(var Value: Variant);
  begin
    inc(ExpStackPtr);
    if ExpStackPtr > High(ExpStack) then
      JvInterpreterError(ieInternal, -1);
    JvInterpreterVarCopy(ExpStack[ExpStackPtr], Value);
  end; { PushExp }

  function PopExp: Variant;
  begin
    if ExpStackPtr = -1 then
      JvInterpreterError(ieInternal, -1);
    JvInterpreterVarCopy(Result, ExpStack[ExpStackPtr]);
    dec(ExpStackPtr);
  end; { PopExp }

  { function Expression called recursively very often, so placing it
    as local function (not class method) improve performance }
  function Expression(const OpTyp: TTokenTyp): Variant;
  var
    Tmp: Variant;
  begin
    Result := Unassigned;
    if OpTyp <> ttUnknown then
      NextToken;
    while True do
    begin
      case TTyp of { }
        ttInteger, ttDouble, ttString, ttFalse, ttTrue, ttIdentifer:
          begin
            Result := Token;
            if TTyp = ttIdentifer then
            begin
              Args.Clear;
              InternalGetValue(nil, 0, Result);
            end;
            NextToken;
            if TTyp in [ttInteger, ttDouble, ttString,
              ttFalse, ttTrue, ttIdentifer] then
              JvInterpreterError(ieMissingOperator, PosEnd {!!});
            if Prior(TTyp) < Prior(OpTyp) then
              Exit;
          end;
        ttMul:
          if priorMul > Prior(OpTyp) then
            Result := PopExp * Expression(TTyp)
          else Exit;
        ttPlus:
         { proceed differently depending on a types }
          if not (PrevTTyp in [ttInteger, ttDouble, ttString, ttFalse, ttTrue,
            ttIdentifer, ttRB, ttRS]) then
           { unar plus }
            Result := Expression(ttNot {highest priority})
          else
            if priorPlus > Prior(OpTyp) then
            begin
              Tmp := PopExp;
              if TVarData(Tmp).VType = varSet then
              begin
                Result := TVarData(Tmp).VInteger or
                  TVarData(Expression(TTyp)).VInteger;
                TVarData(Result).VType := varSet;
              end else
                Result := Tmp + Expression(TTyp)
            end else Exit;
        ttMinus:
         { proceed differently depending on a types }
          if not (PrevTTyp in [ttInteger, ttDouble, ttString, ttFalse, ttTrue,
            ttIdentifer, ttRB, ttRS]) then
           { unar minus }
            Result := -Expression(ttNot {highest priority})
          else
            if priorMinus > Prior(OpTyp) then
            begin
              Tmp := PopExp;
              if TVarData(Tmp).VType = varSet then
              begin
                Result := TVarData(Tmp).VInteger and not
                  TVarData(Expression(TTyp)).VInteger;
                TVarData(Result).VType := varSet;
              end else
                Result := Tmp - Expression(TTyp)
            end else Exit;
        ttDiv:
          if priorDiv > Prior(OpTyp) then
            Result := PopExp / Expression(TTyp)
          else Exit;
        ttIntDiv:
          if priorIntDiv > Prior(OpTyp) then
            Result := PopExp div Expression(TTyp)
          else Exit;
        ttMod:
          if priorMod > Prior(OpTyp) then
            Result := PopExp mod Expression(TTyp)
          else Exit;
        ttOr:
          if priorOr > Prior(OpTyp) then
            Result := PopExp or Expression(TTyp)
          else Exit;
        ttAnd:
          if priorAnd > Prior(OpTyp) then
            Result := PopExp and Expression(TTyp)
          else Exit;
        ttNot:
         { 'Not' has highest priority, so we have not need to check this }
         // if priorNot > Prior(OpTyp) then
          Result := not Expression(TTyp);
         //  else Exit;
        ttEqu:
         { proceed differently depending on a types }
          if priorEqu > Prior(OpTyp) then
          begin
            Tmp := PopExp;
            if TVarData(Tmp).VType in [varObject, varClass, varSet, varPointer] then
              Result := TVarData(Tmp).VInteger =
                TVarData(Expression(TTyp)).VInteger
            else
              Result := Tmp = Expression(TTyp)
          end else Exit;
        ttNotEqu:
         { proceed differently depending on a types }
          if priorNotEqu > Prior(OpTyp) then
          begin
            Tmp := PopExp;
            if TVarData(Tmp).VType in [varObject, varClass, varSet, varPointer] then
              Result := TVarData(Tmp).VInteger <>
                TVarData(Expression(TTyp)).VInteger
            else
              Result := Tmp <> Expression(TTyp)
          end else Exit;
        ttGreater:
          if priorGreater > Prior(OpTyp) then
            Result := PopExp > Expression(TTyp)
          else Exit;
        ttLess:
          if priorLess > Prior(OpTyp) then
            Result := PopExp < Expression(TTyp)
          else Exit;
        ttEquLess:
          if priorEquLess > Prior(OpTyp) then
            Result := PopExp <= Expression(TTyp)
          else Exit;
        ttEquGreater:
          if priorEquGreater > Prior(OpTyp) then
            Result := PopExp >= Expression(TTyp)
          else Exit;
        ttLB:
          begin
            Result := Expression(TTyp);
            if TTyp1 <> ttRB then
              ErrorExpected(''')''');
            NextToken;
          end;
        ttRB:
          if TVarData(Result).VType = varEmpty then
            ErrorExpected(LoadStr2(irExpression))
          else Exit;
        ttLS:
          begin
            NextToken;
            Result := SetExpression1;
            if TTyp1 <> ttRS then
              ErrorExpected(''']''');
            NextToken;
          end;
        ttRS:
          if TVarData(Result).VType = varEmpty then
            ErrorExpected(LoadStr2(irExpression))
          else Exit;
      else
        if TVarData(Result).VType = varEmpty then
          ErrorExpected(LoadStr2(irExpression))
        else Exit;
      end; { case }
      PushExp(Result);
    end;
  end; { Expression }
var
  OldExpStackPtr: Integer;
begin
  Result := Null;
  try
    OldExpStackPtr := ExpStackPtr;
    try
      Expression(ttUnknown);
      JvInterpreterVarCopy(Result, PopExp);
    finally
      ExpStackPtr := OldExpStackPtr;
    end;
  except
    on E: EVariantError do
      JvInterpreterError(ieTypeMistmatch, CurPos);
  end; { try/except }
end; { Expression1 }

function TJvInterpreterExpression.Expression2(const ExpType: Word): Variant;
var
  ErrPos: Integer;
begin
  ErrPos := PosBeg;
  try
    AllowAssignment:= False;
    Result := Expression1;
  finally
    AllowAssignment := True;
  end;
  if TVarData(Result).VType <> ExpType then
    case ExpType of { }
      varInteger:
        JvInterpreterError(ieIntegerRequired, ErrPos);
      varBoolean:
        JvInterpreterError(ieBooleanRequired, ErrPos);
    else
      JvInterpreterError(ieUnknown, ErrPos);
    end; { case }
end; { Expression2 }

{ calulate sets expressions, such as: [fsBold, fsItalic] }

function TJvInterpreterExpression.SetExpression1: Variant;
var
  V1: Variant;
begin
  Result := 0;
  while True do
  begin
    case TTyp of { }
      ttIdentifer, ttInteger:
        begin
          if TTyp = ttInteger then
            Result := Result or Integer(Token)
          else
          begin
            Args.Clear;
            InternalGetValue(nil, 0, V1);
            if VarType(V1) <> varInteger then
              JvInterpreterError(ieIntegerRequired, PosBeg);
            Result := Result or 1 shl Integer(V1);
          end;
          NextToken; { skip ',' }
          if TTyp = ttCol then
            NextToken
          else if TTyp = ttRS then
            Break
          else
            ErrorExpected(''']''');
        end;
      ttRS:
        Break;
    else
      Break;
    end;
  end;
  TVarData(Result).VType := varSet;
end; { SetExpression1 }

procedure TJvInterpreterExpression.ReadArgs;

  function ReadOpenArray: Variant;
  var
    Values: TValueArray;
    i: Integer;
  begin
   { open array or set constant }
    NextToken;
    Values[0] := Expression1;
    i := 1;
    while TTyp = ttCol do
    begin
      NextToken;
      Args.Clear;
      Values[i] := Expression1;
      inc(i);
    end; { while }
    if TTyp <> ttRS then
      ErrorExpected(''']''');
    Result := VarArrayCreate([0, i-1], varVariant);
    for i := 0 to i - 1 do { Iterate }
      Result[i] := Values[i];
    NextToken;
  end;

var
  LocalArgs: TArgs;
  i: Integer;
  SK: TTokenTyp;
begin
  LocalArgs := Args;
  Args := TArgs.Create;
  Args.Indexed := LocalArgs.Indexed;
  try
    i := 0;
    if TTyp = ttLB then
      SK := ttRB
    else { if TTyp = ttLS then }
      SK := ttRS;

    NextToken;
    if TTyp = ttIdentifer then
      LocalArgs.VarNames[i] := Token else
      LocalArgs.VarNames[i] := '';

    Args.Clear;
    if TTyp = ttLS then
      LocalArgs.Values[i] := ReadOpenArray
//added check to recognize C style (), like "NextToken()"
//RWare: if token ')', skip and exit
    else if TTyp = ttRB then begin
      NextToken;
      Exit;
    end else
      JvInterpreterVarCopy(LocalArgs.Values[i], Expression1);

    while TTyp = ttCol do
    begin
      inc(i);
      NextToken;
      if TTyp = ttIdentifer then
        LocalArgs.VarNames[i] := Token else
        LocalArgs.VarNames[i] := '';
      Args.Clear;
      if TTyp = ttLS then
        LocalArgs.Values[i] := ReadOpenArray else
        JvInterpreterVarCopy(LocalArgs.Values[i], Expression1);
    end; { while }
    if TTyp <> SK then
      if SK = ttRB then
        ErrorExpected(''')''') else ErrorExpected(''']''');
    NextToken;
    LocalArgs.Count := i + 1;
  finally { wrap up }
    Args.Free;
    Args := LocalArgs;
  end; { try/finally }
end; { ReadArgs }

procedure TJvInterpreterExpression.InternalGetValue(Obj: Pointer; ObjTyp: Word;
  var Result: Variant);

  procedure UpdateVarParams;
  var
    i, C: Integer;
  begin
    if not Args.HasVars then Exit;
    C := Args.Count;
    Args.Obj := nil;
    Args.ObjTyp := 0;
    Args.Count := 0;
    for i := 0 to C - 1 do { Iterate }
      if (Args.VarNames[i] <> '') and ((Args.Types[i] and varByRef) <> 0) then
      {  if not } SetValue(Args.VarNames[i], Args.Values[i], Args) {then
          JvInterpreterErrorN(ieUnknownIdentifer, PosBeg, Args.VarNames[i])};
    Args.HasVars := False;
  end; { SetVarParams }

var
  Identifer: string;
  V: Variant;
begin
  Identifer := Token;
  NextToken;
  Args.Indexed := TTyp = ttLS;
  if TTyp in [ttLB, ttLS] then
    ReadArgs
  else
    Args.Count := 0;
  Args.Obj := Obj;
  Args.ObjTyp := ObjTyp;
  if (TTyp = ttColon) and AllowAssignment then
  begin
    Back;
    Token1 := Identifer; {!!!!!!!!!!!!!!}
    { Args.Obj, Args.ObjTyp, Args.Count needed in caller }
    Exit;
  end;

  { need result if object field or method or assignment }
  Args.HasResult := (TTyp in [ttPoint, ttRB, ttCol, ttNot..ttEquLess]) or
    Args.Assignment;
  Args.ReturnIndexed := False;

  if GetValue(Identifer, Result, Args) then
  begin
    if TVarData(Result).VType = varRecord then
      if not (FAdapter.SetRecord(Result) or
        (Assigned(GlobalJvInterpreterAdapter) and (FAdapter <> GlobalJvInterpreterAdapter) and
         GlobalJvInterpreterAdapter.SetRecord(Result))) then
        JvInterpreterErrorN(ieRecordNotDefined, -1, 'Unknown RecordType');
   { Args.HasVars may be changed in previous call to GetValue }
    if Args.HasVars then
      UpdateVarParams;
    if Args.Indexed and not Args.ReturnIndexed then
    begin
      if not GetElement(Result, Result, Args) then
       { problem }
        JvInterpreterError(ieArrayRequired, PosBeg);
    end;
  end
  else
    JvInterpreterErrorN(ieUnknownIdentifer, PosBeg {?}, Identifer);

  Args.Obj := nil;
  Args.ObjTyp := 0;
  Args.Count := 0;
 { Args.Obj, Args.ObjTyp, Args.Count NOT needed in caller }

  if TTyp = ttPoint then { object field or method }
  begin
    NextToken;
    if TTyp <> ttIdentifer then
      ErrorExpected(LoadStr2(irIdentifer));
    if not (TVarData(Result).VType in
      [varObject, varClass, varRecord, varDispatch]) then
      JvInterpreterError(ieROCRequired, PosBeg);

    V := Null;
    InternalGetValue(TVarData(Result).vPointer, TVarData(Result).VType, V);
    JvInterpreterVarCopy(Result, V);

    NextToken;
  end;

  Back;
end; { InternalGetValue }

function TJvInterpreterExpression.GetElement(const Variable: Variant; var Value: Variant;
  var Args: TArgs): Boolean;
var
  II2: Integer;
  VV: TJvInterpreterArrayValues;
  PP: PJvInterpreterArrayRec;
  Bound: Integer;
begin
  Result := False;
  if Args.Count <> 0 then
  begin
    case TVarData(Variable).VType of
      varString:
        begin
          if Args.Count > 1 then
            JvInterpreterError(ieArrayTooManyParams, -1);
            if Length(Variable) = 0 then
              raise ERangeError.Create('range check error');
          Value := string(Variable)[integer(Args.Values[0])];
          Result := True;
        end;
      varArray:
        begin
          {Get array value}
          PP := PJvInterpreterArrayRec(Integer(JvInterpreterVarAsType(Variable, varInteger)));
          if Args.Count > PP.Dimension then
            JvInterpreterError(ieArrayTooManyParams, -1)
          else if Args.Count < PP.Dimension then
            JvInterpreterError(ieArrayNotEnoughParams, -1);
          for II2 := 0 to Args.Count - 1 do
          begin
            Bound := Args.Values[II2];
            if Bound < PP.BeginPos[II2] then
              JvInterpreterError(ieArrayIndexOutOfBounds, -1)
            else if Bound > PP.EndPos[II2] then
              JvInterpreterError(ieArrayIndexOutOfBounds, -1);
            VV[II2] := Args.Values[II2];
          end;
          Value := JvInterpreterArrayGetElement(VV, PP);
          Result := True;
        end;
      varObject, varClass:
        begin
          Result := FAdapter.GetElement(Self, Variable, Value, Args);
          if not Result and Assigned(FSharedAdapter) then
            Result := FSharedAdapter.GetElement(Self, Variable, Value, Args);
        end;
      else
       { problem }
        JvInterpreterError(ieArrayRequired, CurPos);
    end;
  end;
end;

function TJvInterpreterExpression.SetElement(var Variable: Variant; const Value: Variant;
  var Args: TArgs): Boolean;
var
  II2: Integer;
  VV: TJvInterpreterArrayValues;
  PP: PJvInterpreterArrayRec;
  Bound: Integer;
begin
  Result := False;
  if Args.Count <> 0 then
  begin
    case TVarData(Variable).VType of
      varString:
        begin
          if Args.Count > 1 then
            JvInterpreterError(ieArrayTooManyParams, -1);
          string(TVarData(Variable).vString)[integer(Args.Values[0])] := string(Value)[1];
          Result := True;
        end;
      varArray:
        begin
          {Get array value}
          PP := PJvInterpreterArrayRec(Integer(JvInterpreterVarAsType(Variable, varInteger)));
          if Args.Count > PP.Dimension then
            JvInterpreterError(ieArrayTooManyParams, -1)
          else if Args.Count < PP.Dimension then
            JvInterpreterError(ieArrayNotEnoughParams, -1);
          for II2 := 0 to Args.Count - 1 do
          begin
            Bound := Args.Values[II2];
            if Bound < PP.BeginPos[II2] then
              JvInterpreterError(ieArrayIndexOutOfBounds, -1)
            else if Bound > PP.EndPos[II2] then
              JvInterpreterError(ieArrayIndexOutOfBounds, -1);
            VV[II2] := Args.Values[II2];
          end;
          JvInterpreterArraySetElement(VV, Value, PP);
          Result := True;
        end;
      varObject, varClass:
        begin
          Result := FAdapter.SetElement(Self, Variable, Value, Args);
          if not Result and Assigned(FSharedAdapter) then
            Result := FSharedAdapter.SetElement(Self, Variable, Value, Args);
        end;
      else
       { problem }
        JvInterpreterError(ieArrayRequired, CurPos);
    end;
  end;
end;

function TJvInterpreterExpression.GetValue(Identifer: string; var Value: Variant;
  var Args: TArgs): Boolean;
begin
  try
    Result := FAdapter.GetValue(Self, Identifer, Value, Args);
    if not Result and Assigned(FSharedAdapter) then
      Result := FSharedAdapter.GetValue(Self, Identifer, Value, Args);
  except
    on E: Exception do
    begin
      UpdateExceptionPos(E, '');
      raise;
    end;
  end;
  if not Result and Assigned(FOnGetValue) then
    FOnGetValue(Self, Identifer, Value, Args, Result);
end; { GetValue }

function TJvInterpreterExpression.SetValue(Identifer: string; const Value: Variant;
  var Args: TArgs): Boolean;
begin
  try
    Result := FAdapter.SetValue(Self, Identifer, Value, Args);
    if not Result and Assigned(FSharedAdapter) then
      Result := FSharedAdapter.SetValue(Self, Identifer, Value, Args);
  except
    on E: EJvInterpreterError do
    begin
      E.FErrPos := PosBeg;
      raise;
    end;
  end;
  if not Result and Assigned(FOnSetValue) then
    FOnSetValue(Self, Identifer, Value, Args, Result);
end; { SetValue }

procedure TJvInterpreterExpression.Run;
begin
  Init;
  NextToken;
  FVResult := Expression1;
end; { Run }


{ ************************ TJvInterpreterFunction ************************ }

constructor TJvInterpreterFunction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FunStack := TList.Create;
  SS := TStringList.Create;
  FEventList := TList.Create;
end; { Create }

destructor TJvInterpreterFunction.Destroy;
begin
  SS.Free;
  FunStack.Free;
  ClearList(FEventList);
  FEventList.Free;
  inherited Destroy;
end; { Destroy }

procedure TJvInterpreterFunction.Init;
begin
  inherited Init;
  FBreak := False;
  FContinue := False;
  FunStack.Clear;
  StateStackPtr := -1;
  FCurUnitName := '';
  FCurInstance := nil;
end; { Init }

procedure TJvInterpreterFunction.PushState;
begin
  inc(StateStackPtr);
  if StateStackPtr > High(StateStack) then
    JvInterpreterError(ieInternal, -1);
  StateStack[StateStackPtr].Token := Token1;
  StateStack[StateStackPtr].TTyp := TTyp1;
  StateStack[StateStackPtr].PrevTTyp := PrevTTyp;
  StateStack[StateStackPtr].Backed := FBacked;
  StateStack[StateStackPtr].CurPos := CurPos;
  StateStack[StateStackPtr].AllowAssignment := AllowAssignment;
end; { PushState }

procedure TJvInterpreterFunction.PopState;
begin
  if StateStackPtr = -1 then
    JvInterpreterError(ieInternal, -1);
  CurPos := StateStack[StateStackPtr].CurPos;
  Token1 := StateStack[StateStackPtr].Token;
  TTyp1 := StateStack[StateStackPtr].TTyp;
  PrevTTyp := StateStack[StateStackPtr].PrevTTyp;
  FBacked := StateStack[StateStackPtr].Backed;
  AllowAssignment := StateStack[StateStackPtr].AllowAssignment;
  dec(StateStackPtr);
end; { PopState }

procedure TJvInterpreterFunction.RemoveState;
begin
  dec(StateStackPtr);
end; { RemoveState }

function TJvInterpreterFunction.GetLocalVars: TJvInterpreterVarList;
begin
  if (FunContext<>nil) then
    Result := PFunContext(FunContext).LocalVars
  else
    Result := nil;
end;

procedure TJvInterpreterFunction.InFunction1(FunDesc: TJvInterpreterFunDesc);
var
  FunArgs: TArgs;
  VarNames: PNameArray;

  procedure EnterFunction;
  var
    FC: PFunContext;
    i: Integer;
  begin
    New(PFunContext(FC));
    FillChar(FC^, sizeof(FC^), 0);
    New(VarNames);
    PFunContext(FC).PrevFunContext := FunContext;
    FunContext := FC;
    PFunContext(FunContext).LocalVars := TJvInterpreterVarList.Create;
    FunStack.Add(FunContext);
    FVResult := Null;
    if FunDesc <> nil then
    begin
      Args.HasVars := False;
      Args.Types := FunDesc.FParamTypes;
      for i := 0 to Args.Count - 1 do { Iterate }
      begin
        PFunContext(FunContext).LocalVars.AddVar('', FunDesc.FParamNames[i], '',
          FunDesc.FParamTypes[i], Args.Values[i]);
        VarNames^ := FunDesc.FParamNames;
        Args.HasVars := Args.HasVars or ((FunDesc.FParamTypes[i] and varByRef)
          <> 0);
      end;
      if FunDesc.ResTyp > 0 then
        PFunContext(FunContext).LocalVars.AddVar('', 'Result', '',
          FunDesc.ResTyp, Var2Type(Null, FunDesc.ResTyp));
    end
    else
      PFunContext(FunContext).LocalVars.AddVar('', 'Result', '', varVariant,
        Null);
    FunArgs := Args;
    Args := TArgs.Create;
  end; { EnterFunction }

  procedure LeaveFunction(Ok: boolean);

    procedure UpdateVarParams;
    var
      i, C: Integer;
    begin
      if not Args.HasVars then Exit;
      C := Args.Count;
      Args.Obj := nil;
      Args.ObjTyp := 0;
      Args.Count := 0;
      for i := 0 to C - 1 do { Iterate }
        if (VarNames[i] <> '') and
          ((Args.Types[i] and varByRef) <> 0) then
          GetValue(VarNames[i], Args.Values[i], Args);
    end; { SetVarParams }

  var
    FC: PFunContext;
    C: Integer;
  begin
    Args.Free;
    Args := FunArgs;
    if Ok then
    begin
      C := Args.Count;
      UpdateVarParams;
      Args.Count := 0;
      if (FunDesc = nil) or (FunDesc.ResTyp > 0) then
        PFunContext(FunContext).LocalVars.GetValue('Result', FVResult, Args);
      Args.Count := C;
    end;
    FC := PFunContext(FunContext).PrevFunContext;
    PFunContext(FunContext).LocalVars.Free;
    Dispose(PFunContext(FunContext));
    Dispose(VarNames);
    FunStack.Delete(FunStack.Count - 1);
    FunContext := FC;
  end; { LeaveFunction }

begin
 { allocate stack }
  EnterFunction;
  try
    FExit := False;
    while True do
    begin
      case TTyp of { }
        ttBegin:
          begin
            Begin1;
            if (TTyp <> ttSemicolon) and not FExit then
              ErrorExpected(''';''');
            Break;
          end;
        ttVar:
          Var1(PFunContext(FunContext).LocalVars.AddVar);
        ttConst:
          Const1(PFunContext(FunContext).LocalVars.AddVar);
      else
        ErrorExpected('''' + kwBEGIN + '''');
      end; { case }
      NextToken;
    end; { while }
    LeaveFunction(True);
    FExit := False;
  except
    on E: Exception do
    begin
     { if (E is EJvInterpreterError) and (Fun <> nil) and
        ((E as EJvInterpreterError).ErrUnitName = '') then }
      if FunDesc <> nil then
        UpdateExceptionPos(E, FunDesc.UnitName)
      else
        UpdateExceptionPos(E, '');
      LeaveFunction(False);
      FExit := False;
      raise;
    end;
  end; { try/finally }
end; { InFunction1 }

function TJvInterpreterFunction.GetValue(Identifer: string; var Value: Variant;
  var Args: TArgs): Boolean;
begin
  Result := False;
 { check in local variables }
  try
    if FunContext <> nil then
      Result := PFunContext(FunContext).LocalVars.GetValue(Identifer, Value, Args);
  except
    on E: Exception do
    begin
      if Assigned(PFunContext(FunContext).Fun) then
        UpdateExceptionPos(E, PFunContext(FunContext).Fun.UnitName)
      else
        UpdateExceptionPos(E, '');
      raise;
    end;
  end;
  if not Result then
    Result := inherited GetValue(Identifer, Value, Args);
end; { GetValue }

function TJvInterpreterFunction.SetValue(Identifer: string; const Value: Variant;
  var Args: TArgs): Boolean;
begin
  Result := False;
 { check in local variables }
  try
    if FunContext <> nil then
      Result := PFunContext(FunContext).LocalVars.SetValue(Identifer, Value, Args);
  except
    on E: Exception do
    begin
      if Assigned(PFunContext(FunContext).Fun) then
        UpdateExceptionPos(E, PFunContext(FunContext).Fun.UnitName)
      else
        UpdateExceptionPos(E, '');
      raise;
    end;
  end;
  if not Result then
    Result := inherited SetValue(Identifer, Value, Args);
end; { SetValue }

procedure TJvInterpreterFunction.DoOnStatement;
begin
end;

{ exit: current position set to next token }

procedure TJvInterpreterFunction.Statement1;
begin
  DoOnStatement;
  case TTyp of { }
    ttIdentifer:
     { assignment or function call }
      begin
        Identifer1;
        if not (TTyp in [ttSemicolon, ttEnd, ttElse, ttUntil, ttFinally, ttExcept]) then
          ErrorExpected(''';''');
       // Back;
      end;
    ttSemicolon:
      ; // Back;
    ttEnd:
      ; // Back;
    ttBegin:
      Begin1;
    ttIf:
      If1;
    ttElse:
      Exit;
    ttWhile:
      While1;
    ttRepeat:
      Repeat1;
    ttFor:
      For1;
    ttBreak:
      FBreak := True;
    ttContinue:
      FContinue := True;
    ttTry:
      Try1;
    ttRaise:
      Raise1;
    ttExit:
      FExit := True;
    ttCase:
      Case1;
    else
      ErrorExpected(''';''');
  end; { case }
end; { Statement1 }

{ exit: current position set to next token }
{ very simple version, many syntax errors are not found out }

procedure TJvInterpreterFunction.SkipStatement1;
begin
  case TTyp of { }
    ttEmpty:
      ErrorExpected('''' + kwEND + '''');
    ttIdentifer:
      SkipIdentifer1;
    ttSemicolon:
      NextToken;
    ttEnd:
      NextToken;
    ttIf:
      begin
        FindToken1(ttThen);
        NextToken;
        SkipStatement1;
        if TTyp = ttElse then
        begin
          NextToken;
          SkipStatement1;
        end;
        Exit;
      end;
    ttElse:
      Exit;
    ttWhile, ttFor:
      begin
        FindToken1(ttDo);
        NextToken;
        SkipStatement1;
        Exit;
      end;
    ttRepeat:
      begin
        SkipToUntil1;
        SkipIdentifer1;
        Exit;
      end;
    ttBreak, ttContinue:
      NextToken;
    ttBegin:
      begin
        SkipToEnd1;
        Exit;
      end;
    ttTry:
      begin
        SkipToEnd1;
        Exit;
      end;
    ttFunction, ttProcedure:
      ErrorExpected('''' + kwEND + '''');
    ttRaise:
      begin
        NextToken;
        SkipIdentifer1;
      end;
    ttExit:
      NextToken;
    ttCase:
      begin
        SkipToEnd1;
        Exit;
      end;
  end; { case }
end; { SkipStatement1 }

{ out: current position set to token after end }

procedure TJvInterpreterFunction.SkipToEnd1;
begin
  while True do
  begin
    NextToken;
    if TTyp = ttEnd then
    begin
      NextToken;
      Break;
    end
    else if TTyp in [ttBegin, ttTry, ttCase] then
      SkipToEnd1
    else if TTyp = ttEmpty then
      ErrorExpected('''' + kwEND + '''')
    else
      SkipStatement1;
    if TTyp = ttEnd then
    begin
      NextToken;
      Break;
    end;
  end;
end; { SkipToEnd }

{ out: current position set to token after end }

procedure TJvInterpreterFunction.SkipToUntil1;
begin
  while True do
  begin
    NextToken;
    if TTyp = ttUntil then
    begin
      NextToken;
      Break;
    end
    else if TTyp = ttEmpty then
      ErrorExpected('''' + kwUNTIL + '''')
    else
      SkipStatement1;
    if TTyp = ttUntil then
    begin
      NextToken;
      Break;
    end;
  end;
end; { SkipToEnd }

{exit: current position set to next token after assignment or function call }

procedure TJvInterpreterFunction.SkipIdentifer1;
begin
  while True do
    case TTyp of { }
      ttEmpty:
        ErrorExpected('''' + kwEND + '''');
      ttIdentifer..ttBoolean, ttLB, ttRB, ttCol, ttPoint, ttLS, ttRS,
        ttNot..ttEquLess, ttTrue, ttFalse:
        NextToken;
      ttSemicolon, ttEnd, ttElse, ttUntil, ttFinally, ttExcept, ttDo, ttOf:
        Break;
      ttColon:
        { 'case' or assignment }
        begin
          NextToken;
          if TTyp <> ttEqu then
          begin
            Back;
            Break;
          end;
        end;
      else
        ErrorExpected(LoadStr2(irExpression))
    end; { case }
end; { SkipIdentifer1 }

procedure TJvInterpreterFunction.FindToken1(TTyp1: TTokenTyp);
begin
  while not (TTyp in [TTyp1, ttEmpty]) do
    NextToken;
  if TTyp = ttEmpty then
    ErrorExpected('''' + kwEND + '''');
end; { FindToken }

function TJvInterpreterFunction.NewEvent(const UnitName: string; const FunName,
  EventType: string; Instance: TObject): TSimpleEvent;
begin
  Result := FAdapter.NewEvent(UnitName, FunName, EventType, Self, Instance);
  if not Assigned(Result) then
    Result := GlobalJvInterpreterAdapter.NewEvent(UnitName, FunName, EventType, Self, Instance);
  if not Assigned(Result) then
    JvInterpreterErrorN(ieEventNotRegistered, -1, EventType);
end; { NewEvent }

procedure TJvInterpreterFunction.InternalSetValue(const Identifer: string);
var
  FunDesc: TJvInterpreterFunDesc;
  PropInf: PPropInfo;
  FunName: string;
  PopSt: Boolean;
  MyArgs: TArgs;
  Variable: Variant;
  Method: TMethod;
begin
  { may be event assignment }
  if (Args.Obj <> nil) and (Args.ObjTyp = varObject) then
  begin
    FunDesc := FAdapter.FindFunDesc(FCurUnitName, Token);
    if FunDesc <> nil then
    begin
      PushState;
      PopSt := True;
      try
        NextToken;
        if not (TTyp in [ttFirstExpression..ttLastExpression] - [ttSemicolon]) then
        begin
          FunName := Token;
          PropInf := GetPropInfo(Args.Obj.ClassInfo, Identifer);
          if Assigned(PropInf) and (PropInf.PropType^.Kind = tkMethod) then
          begin
           { method assignment }
            Method := TMethod(NewEvent(FCurUnitName, FunName,
              PropInf^.PropType^.Name, FCurInstance));
            SetMethodProp(Args.Obj, PropInf, Method);
            FEventList.Add(Method.Data);

            PopSt := False;
            Exit;
          end
          else
            if FAdapter.IsEvent(Args.Obj, Identifer) then { chek only local adapter }
            begin
              if not SetValue(Identifer, FunName, Args) then
                JvInterpreterErrorN(ieUnknownIdentifer, PosBeg, Identifer);
              PopSt := False;
              Exit;
            end;
        end;
      finally
        if PopSt then
          PopState
        else
          RemoveState;
      end;
      //Exit;
    end;
  end;
 { normal (not method) assignmnent }
 { push args }
  MyArgs := Args;
  Args := TArgs.Create;
  try
    Args.Assignment := True;
    JvInterpreterVarCopy(FVResult, Expression1);
  finally { wrap up }
   { pop args }
    Args.Free;
    Args := MyArgs;
  end; { try/finally }
  if Args.Indexed then
  begin
    MyArgs := TArgs.Create;
    try
      if GetValue(Identifer, Variable, MyArgs) then
      begin
        if not SetElement(Variable, FVResult, Args) then
         { problem }
          JvInterpreterError(ieArrayRequired, PosBeg);
        if (TVarData(Variable).VType = varString) and
            not SetValue(Identifer, Variable, MyArgs) then
          JvInterpreterErrorN(ieUnknownIdentifer, PosBeg, Identifer);
      end
      else if not SetValue(Identifer, FVResult, Args) then
        JvInterpreterErrorN(ieUnknownIdentifer, PosBeg, Identifer);
    finally
      MyArgs.Free;
    end;
  end
  else if not SetValue(Identifer, FVResult, Args) then
    JvInterpreterErrorN(ieUnknownIdentifer, PosBeg, Identifer);
end; { InternalSetValue }

procedure TJvInterpreterFunction.Identifer1;
var
  Identifer: string;
begin
  Identifer := Token;
  Args.Clear;
  NextToken;
  if TTyp <> ttColon then
  begin
    Back;
    Args.Assignment := False;
    InternalGetValue(nil, 0, FVResult);
    Identifer := Token; { Back! }
    NextToken;
  end;
  if TTyp = ttColon then { assignment }
  begin
    NextToken;
    if TTyp <> ttEqu then
      ErrorExpected('''=''');
    NextToken;
    InternalSetValue(Identifer);
  end;
end; { Identifer1 }

{exit: current position set to next token after "end"}
procedure TJvInterpreterFunction.Begin1;
begin
  NextToken;
  while True do
  begin
    case TTyp of { }
      ttEnd:
        begin
          NextToken;
          Exit;
        end;
      ttElse, ttDo:
        ErrorExpected('statement');
      ttSemicolon:
        begin
          DoOnStatement;
          NextToken;
        end;
      ttIdentifer, ttBegin, ttIf, ttWhile, ttFor, ttRepeat,
        ttBreak, ttContinue, ttTry, ttRaise, ttExit, ttCase:
        Statement1;
      else
        ErrorExpected('''' + kwEND + '''');
    end; { case }
    if FBreak or FContinue or FExit then
      Exit;
  end;
end; { Begin1 }

{exit: current position set to next token after if block }

procedure TJvInterpreterFunction.If1;
var
  Condition: Variant;
begin
  NextToken;
  Condition := Expression2(varBoolean);
  if TTyp <> ttThen then
    ErrorExpected('''' + kwTHEN + '''');
  NextToken;
  if TVarData(Condition).VBoolean then
  begin
    Statement1;
   // NextToken; {!!!????}
    if TTyp = ttElse then
    begin
      NextToken;
      SkipStatement1;
     // Back; {!!!????}
    end;
  end
  else
  begin
    SkipStatement1;
    if TTyp = ttElse then
    begin
      NextToken;
      Statement1;
    end
   { else
    if TTyp = ttSemicolon then
    begin
      NextToken;
      if TTyp = ttElse then
        JvInterpreterError(ieNotAllowedBeforeElse, PosBeg)
    end; }
  end;
end; { If1 }

{exit: current position set to next token after loop }

procedure TJvInterpreterFunction.While1;
var
  WhileCurPos: Integer;
  WhilePos: Integer;
  Condition: Variant;
begin
  PushState;
  try
    WhilePos := PosEnd;
    WhileCurPos := CurPos;
    while True do
    begin
      NextToken;
      Condition := Expression1;
      if TVarData(Condition).VType <> varBoolean then
        JvInterpreterError(ieBooleanRequired, WhilePos);
      if TTyp <> ttDo then
        ErrorExpected('''' + kwDO + '''');
      NextToken;
      if TVarData(Condition).VBoolean then
      begin
        FContinue := False;
        FBreak := False;
        Statement1;
        if FBreak or FExit then
          Break;
      end
      else
        Break;
      CurPos := WhileCurPos;
    end; { while }
  finally
    PopState;
  end;
  SkipStatement1;
  FContinue := False;
  FBreak := False;
end; { While1 }

{exit: current position set to next token after loop }

procedure TJvInterpreterFunction.Repeat1;
var
  RepeatCurPos: Integer;
  Condition: Variant;
begin
  RepeatCurPos := CurPos;
  while True do
  begin
    NextToken;
    case TTyp of
      ttElse, ttDo:
        ErrorExpected('statement');
      ttSemicolon:
        DoOnStatement;
      ttIdentifer, ttBegin, ttIf, ttWhile, ttFor, ttRepeat,
        ttBreak, ttContinue, ttTry, ttRaise, ttExit, ttCase:
        begin
          FContinue := False;
          FBreak := False;
          Statement1;
          if FBreak or FExit then
            Break;
        end;
      ttUntil:
        begin
          NextToken;
          Condition := Expression1;
          if TVarData(Condition).VType <> varBoolean then
            JvInterpreterError(ieBooleanRequired, CurPos);
          if TVarData(Condition).VBoolean then
            Break
          else
            CurPos := RepeatCurPos;
        end;
    else
      ErrorExpected('''' + kwUNTIL + '''');
    end;
  end; { while }
  if FBreak or FExit then
  begin
    SkipToUntil1;
    SkipIdentifer1;
  end;
  FContinue := False;
  FBreak := False;
end; { Repeat1 }

{exit: current position set to next token after loop }
procedure TJvInterpreterFunction.For1;
var
  i: Integer;
  DoCurPos: Integer;
  iBeg, iEnd: Integer;
  LoopVariable: string;
begin
  NextToken;
  if TTyp <> ttIdentifer then
    ErrorExpected(LoadStr2(irIdentifer));
 // CheckLocalIdentifer;
  LoopVariable := Token;
  NextToken;
  if TTyp <> ttColon then
    ErrorExpected(''':''');
  NextToken;
  if TTyp <> ttEqu then
    ErrorExpected('''=''');
  NextToken;
  iBeg := Expression2(varInteger);
  if TTyp <> ttTo then
    ErrorExpected('''' + kwTO + '''');
  NextToken;
  iEnd := Expression2(varInteger);
  if TTyp <> ttDo then
    ErrorExpected('''' + kwDO + '''');
  DoCurPos := CurPos;
  NextToken;
  for i := iBeg to iEnd do { Iterate }
  begin
    Args.Clear;
    if not SetValue(LoopVariable, i, Args) then
      JvInterpreterErrorN(ieUnknownIdentifer, PosBeg, LoopVariable);
    FContinue := False;
    FBreak := False;
    Statement1;
    if FBreak or FExit then
    begin
      CurPos := DoCurPos;
      NextToken;
      Break;
    end;
    CurPos := DoCurPos;
    NextToken;
  end; { for }
  SkipStatement1;
  FContinue := False;
  FBreak := False;
end; { For1 }

{exit: current position set to next token after case }
procedure TJvInterpreterFunction.Case1;
var
  Selector, Expression: Integer;
begin
  NextToken;
  Selector := Expression2(varInteger);
  if TTyp <> ttOf then
    ErrorExpected('''' + kwOF + '''');
  while True do
  begin
    NextToken;
    case TTyp of
      ttIdentifer, ttInteger:
        begin
          Expression := Expression2(varInteger);
          if TTyp <> ttColon then
            ErrorExpected('''' + ':' + '''');
          NextToken;
          if Expression = Selector then
          begin
            Statement1;
            SkipToEnd1;
            Break;
          end
          else
            SkipStatement1;
        end;
      ttElse:
        begin
          NextToken;
          Statement1;
          SkipToEnd1;
          Break;
        end;
      ttEnd:
        Break;
      else
        ErrorExpected('''' + kwEND + '''');
    end;
  end;
end;

procedure TJvInterpreterFunction.Var1(AddVarFunc: TJvInterpreterAddVarFunc);
var
  i: Integer;
  Value: Variant;
  TypName: string;
  Typ: Word;
  {----olej----}
  {Temporary for array type}
  ArrayBegin, ArrayEnd: TJvInterpreterArrayValues;
  ArrayType: Integer;
  V: Variant;
  Dimension: Integer;
  Minus: Boolean;
  {----olej----}
begin
  repeat
    Typ := varEmpty;
    ArrayType := varEmpty;
    Dimension := 0;
    SS.Clear;
    repeat
      NextToken;
      if TTyp <> ttIdentifer then
        ErrorExpected(LoadStr2(irIdentifer));
      SS.Add(Token);
      NextToken;
    until TTyp <> ttCol;
    if TTyp <> ttColon then
      ErrorExpected(''':''');
    NextToken;
    TypName := Token;
    if TTyp = ttIdentifer then
      Typ := TypeName2VarTyp(TypName)
    else if TTyp = ttArray then
      Typ := varArray
    else
      ErrorExpected(LoadStr2(irIdentifer));

    {***** olej *****}
    {Get Array variables params}
    {This is code is not very clear}
    if Typ = varArray then
    begin
      NextToken;
      if TTyp <> ttLs then
        ErrorExpected('''[''');
      {Parse Array Range}
      Dimension := 0;
      repeat
        NextToken;
        Minus := False;
        if (Trim(TokenStr1) = '-') then
        begin
          Minus := True;
          NextToken;
        end;
        if Pos('..', TokenStr1) < 1 then
          ErrorExpected('''..''');
        try
          ArrayBegin[Dimension] :=
            StrToInt(Copy(TokenStr1, 1, Pos('..', TokenStr1) - 1));
          ArrayEnd[Dimension] :=
            StrToInt(Copy(TokenStr1, Pos('..', TokenStr1) + 2, Length(TokenStr1)));
          if Minus then ArrayBegin[Dimension] := ArrayBegin[Dimension] * (-1);
        except
          ErrorExpected('''Integer Value''');
        end;
        if (Dimension < 0) or (Dimension > MaxArgs) then
          JvInterpreterError(ieArrayBadDimension, CurPos);
        if not (ArrayBegin[Dimension] <= ArrayEnd[Dimension]) then
          JvInterpreterError(ieArrayBadRange, CurPos);
      {End Array Range}
        NextToken;
        Inc(Dimension);
      until TTyp <> ttCol; { , }

      if TTyp <> ttRs then
        ErrorExpected(''']''');
      NextToken;
      if TTyp <> ttOf then
        ErrorExpected('''' + kwOF + '''');
      NextToken;
      ArrayType := TypeName2VarTyp(Token);
      if ArrayType = varEmpty then
        ErrorNotImplemented(Token + ' array type');
    end;
    {end: var A:array[1..200] of integer, parsing}
    {##### olej #####}
    for i := 0 to SS.Count - 1 do { Iterate }
    begin
      Value := Null;
      TVarData(Value).VType := varEmpty;
     { may be record }
      if not FAdapter.NewRecord(TypName, Value) then
        GlobalJvInterpreterAdapter.NewRecord(TypName, Value);
      {***** olej *****}
      {Create array with arrayType}
      if Typ = varArray then
      begin
        V := Integer(JvInterpreterArrayInit(Dimension, ArrayBegin, ArrayEnd, ArrayType));
        TVarData(V).VType := varArray;
        AddVarFunc(FCurUnitName, SS[i], TypName, varArray, V);
      end
      else
      begin
        if (TVarData(Value).VType = varEmpty) and (Typ <> 0) then
          Value := Var2Type(Value, Typ);
        AddVarFunc(FCurUnitName, SS[i], TypName, Typ, Value);
      end;
     {end - array is allocated}
     {##### olej #####}
    end;
    SS.Clear;
    NextToken;
    if TTyp <> ttSemicolon then
      ErrorExpected(''';''');
    NextToken;
    Back;
  until TTyp <> ttIdentifer;
end; { Var1 }

procedure TJvInterpreterFunction.Const1(AddVarFunc: TJvInterpreterAddVarFunc);
var
  Identifer: string;
  Value: Variant;
begin
  repeat
    NextToken;
    if TTyp <> ttIdentifer then
      ErrorExpected(LoadStr2(irIdentifer));
    Identifer := Token;
    NextToken;
    if TTyp <> ttEqu then
      ErrorExpected('=');
    NextToken;
    Value := Expression1;

    AddVarFunc(FCurUnitName, Identifer, '', varEmpty, Value);
    if TTyp <> ttSemicolon then
      ErrorExpected(''';''');
    NextToken;
    Back;
  until TTyp <> ttIdentifer;
end; { Const1 }

procedure TJvInterpreterFunction.Try1;

var
  ReRaiseException: Boolean;

  procedure FindFinallyExcept;
  begin
    while True do
    begin
      case TTyp of { }
        ttEmpty:
          ErrorExpected('''' + kwEND + '''');
        ttSemicolon: ;
        ttFinally, ttExcept:
          Exit;
      else
        SkipStatement1;
      end;
      NextToken;
    end;
  end; { FindFinallyExcept }

  procedure Except1(E: Exception);
  var
    ExceptionClassName, ExceptionVarName: string;
    ExceptionClass: TClass;
    V: Variant;

    function On1: boolean;
    begin
      NextToken;
      if TTyp <> ttIdentifer then
        ErrorExpected(LoadStr2(irIdentifer));
      ExceptionClassName := Token;
      NextToken;
      if TTyp = ttColon then
      begin
        NextToken;
        if TTyp <> ttIdentifer then
          ErrorExpected(LoadStr2(irIdentifer));
        ExceptionVarName := ExceptionClassName;
        ExceptionClassName := Token;
        NextToken;
      end;
      Args.Clear;
      if not GetValue(ExceptionClassName, V, Args) then
        JvInterpreterErrorN(ieUnknownIdentifer, PosBeg {?}, ExceptionClassName);
      if VarType(V) <> varClass then
        JvInterpreterError(ieClassRequired, PosBeg {?});
      ExceptionClass := V2C(V);
      if TTyp <> ttDo then
        ErrorExpected('''' + kwDO + '''');
      Result := E is ExceptionClass;
      if Result then
       { do this 'on' section }
      begin
        NextToken;
        PFunContext(FunContext).LocalVars.AddVar('', ExceptionVarName,
          ExceptionClassName, varObject, O2V(E));
        try
          Statement1;
        finally { wrap up }
          PFunContext(FunContext).LocalVars.DeleteVar('', ExceptionVarName);
        end; { try/finally }
        SkipToEnd1;
      end
      else
      begin
        NextToken;
        SkipStatement1;
       { if TTyp = ttSemicolon then
          NextToken; }
      end;
    end; { On1 }

  begin
    NextToken;
    if TTyp = ttOn then
    begin
      if On1 then
      begin
        ReRaiseException := False;
        Exit;
      end;
      while True do
      begin
        NextToken;
        case TTyp of { }
          ttEmpty:
            ErrorExpected('''' + kwEND + '''');
          ttOn:
            if On1 then
            begin
              ReRaiseException := False;
              Exit;
            end;
          ttEnd:
            begin
              ReRaiseException := True;
              Exit;
            end;
          ttElse:
            begin
              NextToken;
              Statement1;
              NextToken;
              if TTyp = ttSemicolon then
                NextToken;
              if TTyp <> ttEnd then
                ErrorExpected('''' + kwEND + '''');
              Exit;
            end;
        else
          ErrorExpected('''' + kwEND + '''');
        end; { case }
      end; { while }
    end
    else
    begin
      Back;
      Begin1;
    end;
  end; { Except1 }

  procedure DoFinallyExcept(E: Exception);
  begin
    case TTyp of { }
      ttFinally:
       { do statements up to 'end' }
        begin
          Begin1;
          if E <> nil then
          begin
            ReRaiseException := True;
          end;
        end;
      ttExcept:
        begin
          if E = nil then
           { skip except section }
            SkipToEnd1
          else
           { except section }
          begin
            try
              Except1(E);
            except
              on E1: EJvInterpreterError do
                if E1.ErrCode = ieRaise then
                  ReRaiseException := True;
                else
                  raise;
            end;
          end;
        end;
    end;
  end; { DoFinallyExcept }

begin
  while True do
  begin
    NextToken;
    case TTyp of { }
      ttFinally:
        begin
          DoFinallyExcept(nil);
          Exit;
        end;
      ttExcept:
        begin
          DoFinallyExcept(nil);
          Exit;
        end;
      ttSemicolon:
        DoOnStatement;
      ttIdentifer, ttBegin, ttIf, ttWhile, ttFor, ttRepeat,
        ttBreak, ttContinue, ttTry, ttRaise, ttExit, ttCase:
        begin
          try
            Statement1;
            if FBreak or FContinue or FExit then
            begin
              FindFinallyExcept;
              DoFinallyExcept(nil);
              Exit;
            end;
          except
            on E: Exception do
            begin
              FindFinallyExcept;

              ReRaiseException := False;
              DoFinallyExcept(E);

              if ReRaiseException then
                raise
              else
                Exit;
            end;
          end; { try/finally }
        end;
    else
      ErrorExpected('''' + kwFINALLY + '''');
    end; { case }
  end;
end; { Try1 }

procedure TJvInterpreterFunction.Raise1;
var
  V: Variant;
begin
  NextToken;
  case TTyp of
    ttEmpty, ttSemicolon, ttEnd, ttBegin, ttElse, ttFinally, ttExcept:
     { re-raising exception }
      raise EJvInterpreterError.Create(ieRaise, PosBeg, '', '');
    ttIdentifer:
      begin
        InternalGetValue(nil, 0, V);
        if VarType(V) <> varObject then
          JvInterpreterError(ieClassRequired, PosBeg {?});
        raise V2O(V);
      end;
  else
    JvInterpreterError(ieClassRequired, PosBeg {?});
  end;
end; { Raise1 }

procedure TJvInterpreterFunction.Run;
begin
  Init;
  NextToken;
  InFunction1(nil);
end; { Run }


{************************ TJvInterpreterUnit **************************}

constructor TJvInterpreterUnit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClearUnits := True;
  FEventHandlerList := TList.Create;
end; { Create }

destructor TJvInterpreterUnit.Destroy;
begin
  ClearList(FEventHandlerList);
  FEventHandlerList.Free;
  inherited Destroy;
end; { Destroy }

procedure TJvInterpreterUnit.Init;
begin
  inherited Init;
  if FClearUnits then
  begin
    FAdapter.ClearSource;
    FUnitSection := usUnknown;
    ClearList(FEventHandlerList);
  end;
end; { Init }

procedure TJvInterpreterUnit.ReadFunHeader(FunDesc: TJvInterpreterFunDesc);
var
  TypName: string;

  procedure ReadParams;
  var
    VarParam: boolean;
    ParamType: string;
    iBeg: integer;
  begin
    while True do
    begin
      VarParam := False;
      NextToken;
      FunDesc.FParamNames[FunDesc.ParamCount] := Token;
      if TTyp = ttRB then
        Break;
      if TTyp = ttVar then
      begin
        VarParam := True;
        NextToken;
      end;
    {  if TTyp = ttConst then
        NextToken; }
      iBeg := FunDesc.ParamCount;
      while True do
      begin
        case TTyp of
          ttIdentifer:
            FunDesc.FParamNames[FunDesc.ParamCount] := Token;
          ttSemicolon: Break;
          ttRB: Exit;
          ttColon:
            begin
              NextToken;
              if TTyp <> ttIdentifer then
                ErrorExpected(LoadStr2(irIdentifer));
              ParamType := Token;
              while True do
              begin
                if TTyp = ttRB then
                  Back;
                if TTyp in [ttRB, ttSemicolon] then
                  Break;
                NextToken;
              end; { while }
              inc(FunDesc.FParamCount);
              while iBeg < FunDesc.FParamCount do
              begin
                FunDesc.FParamTypes[iBeg] := TypeName2VarTyp(ParamType);
                if VarParam then
                  FunDesc.FParamTypes[iBeg] := FunDesc.FParamTypes[iBeg] or
                    varByRef;
                inc(iBeg);
              end;
              Break;
            end;
          ttCol:
            inc(FunDesc.FParamCount);
        end;
        NextToken;
      end; { while }
    end; { while }
  end;

var
  Fun: boolean;
begin
  Fun := TTyp = ttFunction;
  NextToken;
  if TTyp <> ttIdentifer then
    ErrorExpected(LoadStr2(irIdentifer));
  FunDesc.FIdentifer := Token;
  NextToken;
  if TTyp = ttPoint then
  begin
    FunDesc.FClassIdentifer := FunDesc.FIdentifer;
    NextToken;
    if TTyp <> ttIdentifer then
      ErrorExpected(LoadStr2(irIdentifer));
    FunDesc.FIdentifer := Token;
    NextToken;
  end;
  FunDesc.FResTyp := varEmpty;
  FunDesc.FParamCount := 0;
  if TTyp = ttLB then
  begin
   //  NextToken;
    ReadParams;
    NextToken;
  end;
  if Fun then
    if (TTyp = ttColon) then
    begin
      NextToken;
      if TTyp <> ttIdentifer then
        ErrorExpected(LoadStr2(irIdentifer));
      TypName := Token;
      FunDesc.FResTyp := TypeName2VarTyp(TypName);
      if FunDesc.FResTyp = 0 then
        FunDesc.FResTyp := varVariant;
      NextToken;
    end
    else
      ErrorExpected(''':''');
  if TTyp <> ttSemicolon then
    ErrorExpected(''';''');
end; { ReadFunHeader }

procedure TJvInterpreterUnit.Function1;
var
  FunDesc: TJvInterpreterFunDesc;
  FunName: string;
  FunIndex: integer;
  DllName: string;
  LastTTyp:TTokenTyp; // Ivan_ra
begin
  FunDesc := TJvInterpreterFunDesc.Create;
  try
    ReadFunHeader(FunDesc);
    FunDesc.FPosBeg := CurPos;
    LastTTyp:=TTyp; // Ivan_ra
    NextToken;
    if TTyp = ttExternal then
    begin
      NextToken;
      if TTyp = ttString then
        DllName := Token
      else if TTyp = ttIdentifer then
      begin
        Args.Clear;
        if not GetValue(Token, FVResult, Args) then
          JvInterpreterErrorN(ieUnknownIdentifer, PosBeg, Token);
        DllName := vResult;
      end
      else
        ErrorExpected('''string constant'''); {DEBUG!!!}
      NextToken;
      if TTyp <> ttIdentifer then
        ErrorExpected('''name'' or ''index''');
      FunIndex := -1;
      FunName := '';
      if Cmp(Token, 'name') then
      begin
        NextToken;
        if TTyp = ttString then
          FunName := Token
        else
          ErrorExpected('''string constant'''); {DEBUG!!!}
      end
      else
        if Cmp(Token, 'index') then
        begin
          NextToken;
          if TTyp = ttInteger then
            FunIndex := Token
          else
            ErrorExpected('''integer constant'''); {DEBUG!!!}
        end
        else
          ErrorExpected('''name'' or ''index''');
      with FunDesc do
        FAdapter.AddExtFun(FCurUnitName {??!!}, FIdentifer, noInstance, DllName,
          FunName, FunIndex, FParamCount, FParamTypes, FResTyp);
      NextToken;
    end
    // Ivan_ra start
    else
    if FUnitSection = usInterface then begin
      CurPos:=FunDesc.FPosBeg;
      TTyp1:=LastTTyp;
    end
    // Ivan_ra finish
    else
    begin
      FindToken1(ttBegin);
      SkipToEnd1;
      with FunDesc do
        FAdapter.AddSrcFun(FCurUnitName {??!!}, FIdentifer, FPosBeg, CurPos,
          FParamCount, FParamTypes, FParamNames, FResTyp, nil);
    end;
  finally
    FunDesc.Free;
  end;
end; { Function1 }

procedure TJvInterpreterUnit.ReadUnit(const UnitName: string);
var
  OldUnitName: string;
  OldSource: string;
  S: string;
begin
  if FAdapter.UnitExists(UnitName) then Exit;
  FAdapter.AddSrcUnit(FCurUnitName, '', '');
  OldUnitName := FCurUnitName;
  OldSource := Source;
  PushState;
  try
    try
      if not GetUnitSource(UnitName, S) then
        JvInterpreterErrorN(ieUnitNotFound, PosBeg, UnitName);
      FCurUnitName := UnitName;
      Source := S;
      NextToken;
      if TTyp <> ttUnit then
        ErrorExpected('''' + kwUNIT + '''');
      Unit1;
    except
      on E: Exception do
      begin
        UpdateExceptionPos(E, FCurUnitName);
        raise;
      end;
    end
  finally { wrap up }
    FCurUnitName := OldUnitName;
    Source := OldSource;
    PopState;
  end; { try/finally }
end; { ReadUnit }

procedure TJvInterpreterUnit.Uses1(var UsesList: string);
begin
  NextToken;
  if not (TTyp in [ttIdentifer, ttString]) then
    ErrorExpected(LoadStr2(irIdentifer));
  UsesList := Token;
  ReadUnit(Token);
  while True do
  begin
    NextToken;
    if TTyp = ttIn then
    begin
      { ignore }
      NextToken;
      NextToken;
    end;
    if TTyp = ttSemicolon then
      Exit;
    if TTyp <> ttCol then
      ErrorExpected(''',''');
    NextToken;
    if not (TTyp in [ttIdentifer, ttString]) then
      ErrorExpected(LoadStr2(irIdentifer));
    UsesList := UsesList + ',';
    ReadUnit(Token);
  end; { while }
end; { Uses1 }

procedure TJvInterpreterUnit.Unit1;
var
  UsesList: string;
begin
  NextToken;
  if TTyp <> ttIdentifer then
    ErrorExpected(LoadStr2(irIdentifer));
  FCurUnitName := Token;
  NextToken;
  if TTyp <> ttSemicolon then
    ErrorExpected(''';''');
  UsesList := '';
  NextToken;
  while True do
  begin
    case TTyp of { }
      ttEmpty:
        ErrorExpected('''' + kwEND + '''');
      ttFunction, ttProcedure:
        begin
          Function1;
          if TTyp <> ttSemicolon then
            ErrorExpected(''';''');
        end;
      ttEnd:
        Break;
      ttUses:
        Uses1(UsesList);
      ttVar:
        Var1(FAdapter.AddSrcVar);
      ttConst:
        Const1(FAdapter.AddSrcVar);
      ttInterface:
        FUnitSection := usInterface;
      ttImplementation:
        FUnitSection := usImplementation;
      ttType:
        Type1;
      else
        ErrorExpected(LoadStr2(irDeclaration));
    end; { case }
    NextToken;
  end; { while }
  if TTyp <> ttEnd then
    ErrorExpected('''' + kwEND + '''');
  NextToken;
  if TTyp <> ttPoint then
    ErrorExpected('''.''');
  FAdapter.AddSrcUnit(FCurUnitName, Source, UsesList);
end; { Unit1 }

procedure TJvInterpreterUnit.Type1;
var
  Identifer: string;
begin
  NextToken;
  if TTyp <> ttIdentifer then
    ErrorExpected(LoadStr2(irIdentifer));
  Identifer := Token;
  NextToken;
  if TTyp <> ttEqu then
    ErrorExpected('''=''');
  NextToken;
  case TTyp of
    ttClass:
      Class1(Identifer);
    else
     { only class declaration for form is supported }
      ErrorExpected(LoadStr2(irClass));
  end;
end;

procedure TJvInterpreterUnit.Class1(const Identifer: string);
var
  JvInterpreterSrcClass: TJvInterpreterIdentifer;
begin
  NextToken;
  if TTyp <> ttLB then
    ErrorExpected('''(''');
  NextToken;
  if TTyp <> ttIdentifer then
    ErrorExpected(LoadStr2(irIdentifer));
  NextToken;
  if TTyp <> ttRB then
    ErrorExpected(''')''');
  FindToken1(ttEnd);
  NextToken;
  if TTyp <> ttSemicolon then
    ErrorExpected(''';''');
  JvInterpreterSrcClass := TJvInterpreterIdentifer.Create;
  JvInterpreterSrcClass.UnitName := FCurUnitName;
  JvInterpreterSrcClass.Identifer := Identifer;
  FAdapter.AddSrcClass(JvInterpreterSrcClass);
end;

procedure TJvInterpreterUnit.Run;
var
  FunDesc: TJvInterpreterFunDesc;
begin
  Init;
  NextToken;
  case TTyp of { }
    ttVar, ttBegin:
      InFunction1(nil);
    ttFunction, ttProcedure:
      Function1;
    ttUnit:
      begin
        try
          Unit1;
        except
          on E: Exception do
          begin
            UpdateExceptionPos(E, FCurUnitName);
            raise;
          end;
        end;
        FCompiled := True;
       { execute main function }
        FunDesc := FAdapter.FindFunDesc(FCurUnitName, 'main');
        if FunDesc = nil then
          JvInterpreterError(ieMainUndefined, -1);
        CurPos := FunDesc.PosBeg;
        NextToken;
        InFunction1(FunDesc);
      end;
    else
      FVResult := Expression1;
  end; { case }
  FCompiled := True;
end; { Run }

procedure TJvInterpreterUnit.Compile;
begin
  Init;
  try
    NextToken;
    if TTyp <> ttUnit then
      ErrorExpected('''' + kwUNIT + '''');
    Unit1;
  except
    on E: Exception do
    begin
      UpdateExceptionPos(E, FCurUnitName);
      raise;
    end;
  end;
  FCompiled := True;
end; { Compile }

procedure TJvInterpreterUnit.SourceChanged;
begin
  inherited SourceChanged;
end; { SourceChanged }

function TJvInterpreterUnit.GetValue(Identifer: string; var Value: Variant;
  var Args: TArgs): Boolean;
var
  FunDesc: TJvInterpreterFunDesc;
  OldArgs: TArgs;
begin
  Result := inherited GetValue(Identifer, Value, Args);
  if Result then Exit;
  if Args.Obj = nil then
    FunDesc := FAdapter.FindFunDesc(FCurUnitName, Identifer)
  else if (Args.Obj is TJvInterpreterSrcUnit) then
    FunDesc := FAdapter.FindFunDesc((Args.Obj as TJvInterpreterSrcUnit).Identifer,
      Identifer)
  else
    FunDesc := nil;

  Result := FunDesc <> nil;
  if Result then
  begin
    FAdapter.CheckArgs(Args, FunDesc.FParamCount, FunDesc.FParamTypes); {not tested !}
    OldArgs := Self.Args;
    try
      Self.Args := Args;
      ExecFunction(FunDesc);
    finally
      Self.Args := OldArgs;
    end;
    Value := FVResult;
  end;
end; { GetValue }

function TJvInterpreterUnit.SetValue(Identifer: string; const Value: Variant;
  var Args: TArgs): Boolean;
begin
  Result := inherited SetValue(Identifer, Value, Args);
end; { SetValue }

function TJvInterpreterUnit.GetUnitSource(UnitName: string; var Source: string): boolean;
begin
  Result := False;
  if Assigned(FOnGetUnitSource) then
    FOnGetUnitSource(UnitName, Source, Result);
end; { GetUnitSource }

procedure TJvInterpreterUnit.DeclareExternalFunction(const Declaration: string);
var
  OldSource: string;
  OldPos: integer;
begin
  Source := Declaration;
  OldSource := Source;
  OldPos := Parser.Pos;
  try
    NextToken;
    if not (TTyp in [ttFunction, ttProcedure]) then
      ErrorExpected('''' + kwFUNCTION + ''' or ''' + kwPROCEDURE + '''');
    Function1;
  finally { wrap up }
    Source := OldSource;
    Parser.Pos := OldPos;
  end; { try/finally }
end; { DeclareExternalFunction }

procedure TJvInterpreterUnit.ExecFunction(Fun: TJvInterpreterFunDesc);
var
  OldUnitName: string;
  S: string;
begin
  PushState;
  AllowAssignment := True;
  OldUnitName := FCurUnitName;
  try
    if not Cmp(FCurUnitName, Fun.UnitName) then
    begin
      FCurUnitName := Fun.UnitName;
      FAdapter.CurUnitChanged(FCurUnitName, S);
      Source := S;
    end;
    CurPos := Fun.PosBeg;
    NextToken;
    try
      InFunction1(Fun);
    except
      on E: Exception do
      begin
        UpdateExceptionPos(E, FCurUnitName);
        raise;
      end;
    end;
  finally { wrap up }
    if not Cmp(FCurUnitName, OldUnitName) then
    begin
      FCurUnitName := OldUnitName;
      FAdapter.CurUnitChanged(FCurUnitName, S);
      Source := S;
    end;
    PopState;
  end; { try/finally }
end; { ExecFunction }

function TJvInterpreterUnit.CallFunction(const FunName: string; Args: TArgs;
  Params: array of Variant): Variant;
begin
  Result := CallFunctionEx(nil, '', FunName, Args, Params);
end; { CallFunction }

function TJvInterpreterUnit.CallFunctionEx(Instance: TObject; const UnitName: string;
  const FunName: string; Args: TArgs; Params: array of Variant): Variant;
var
  FunDesc: TJvInterpreterFunDesc;
  i: Integer;
  OldArgs: TArgs;
  OldInstance: TObject;
begin
  if not FCompiled then Compile;
  OldInstance := FCurInstance;
  try
    FCurInstance := Instance;
    FunDesc := FAdapter.FindFunDesc(UnitName, FunName);
    if FunDesc <> nil then
    begin
      OldArgs := Self.Args;
      if Args = nil then
      begin
        Self.Args.Clear;
        for i := Low(Params) to High(Params) do { Iterate }
        begin
          Self.Args.Values[Self.Args.Count] := Params[i];
          inc(Self.Args.Count);
        end; { for }
      end
      else
        Self.Args := Args;
      try
       { simple init }
        FBreak := False;
        FContinue := False;
        FLastError.Clear;

        ExecFunction(FunDesc);

        Result := FVResult;
      finally
        Self.Args := OldArgs;
      end;
    end
    else
      JvInterpreterErrorN(ieUnknownIdentifer, -1, FunName);
  finally
    FCurInstance := OldInstance;
  end;
end; { CallFunctionEx }

function TJvInterpreterUnit.FunctionExists(const UnitName: string; const FunName: string)
  : boolean;
begin
  Result := FAdapter.FindFunDesc(UnitName, FunName) <> nil;
end; { FunctionExists }

{######################## TJvInterpreterUnit ##########################}


{*********************** TJvInterpreterProgram ***********************}

type
  TJvInterpreterProgramStrings = class(TStringList)
  private
    FJvInterpreterProgram: TJvInterpreterProgram;
  protected
    procedure Changed; override;
  end;

procedure TJvInterpreterProgramStrings.Changed;
begin
  FJvInterpreterProgram.Source := Text;
end;

constructor TJvInterpreterProgram.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPas := TJvInterpreterProgramStrings.Create;
  (FPas as TJvInterpreterProgramStrings).FJvInterpreterProgram := Self;
end; { Create }

destructor TJvInterpreterProgram.Destroy;
begin
  FPas.Free;
  inherited Destroy;
end; { Destroy }

procedure TJvInterpreterProgram.SetPas(Value: TStrings);
begin
  FPas.Assign(Value);
end;

procedure TJvInterpreterProgram.DoOnStatement;
begin
  if Assigned(FOnStatement) then
    FOnStatement(Self);
end;

procedure TJvInterpreterProgram.Run;
var
  UsesList: string;
begin
  if AnsiStrLIComp(PChar(Parser.Source), 'program ', Length('program ')) <> 0 then
  begin
    inherited Run;
    Exit;
  end;
  Init;
  NextToken;
  while True do
  begin
    case TTyp of { }
      ttEmpty:
        ErrorExpected('''' + kwEND + '''');
      ttFunction, ttProcedure:
        begin
          Function1;
          if TTyp <> ttSemicolon then
            ErrorExpected(''';''');
        end;
      ttEnd:
        Break;
      ttUses:
        Uses1(UsesList);
      ttVar:
        Var1(FAdapter.AddSrcVar);
      ttConst:
        Const1(FAdapter.AddSrcVar);
      ttInterface:
        FUnitSection := usInterface;
      ttImplementation:
        FUnitSection := usImplementation;
      ttType:
        Type1;
      ttProgram:
        begin
          NextToken;
          FCurUnitName := Token;
          NextToken;
          if TTyp <> ttSemicolon then
            ErrorExpected(''';''');
        end;
      ttBegin:
        Break;
      else
        ErrorExpected('''' + kwEND + '''');
    end; { case }
    NextToken;
  end;
  FCompiled := True;
  FAdapter.AddSrcUnit(FCurUnitName, Source, UsesList);
 { execute program function }
{  FunDesc := FAdapter.FindFunDesc(FCurUnitName, 'program');
  if FunDesc <> nil then
  begin
    CurPos := FunDesc.PosBeg;
    NextToken;
    InFunction1(FunDesc);
  end; }
  try
    Begin1;
    if (TTyp <> ttPoint) then
      ErrorExpected('''.''');
  except
    on E: Exception do
    begin
      UpdateExceptionPos(E, FCurUnitName);
      raise;
    end;
  end;
end; { Run }


{$IFDEF JvInterpreter_OLEAUTO}
var
  OleInitialized: Boolean;
{$ENDIF JvInterpreter_OLEAUTO}
{ TJvInterpreterFunDesc }

initialization
  GlobalJvInterpreterAdapter := TJvInterpreterAdapter.Create(nil);
{$IFDEF JvInterpreter_OLEAUTO}
  OleInitialized := OleInitialize(nil) = S_OK;
{$ENDIF JvInterpreter_OLEAUTO}
finalization
{$IFDEF JvInterpreter_OLEAUTO}
  if OleInitialized then OleUnInitialize;
{$ENDIF JvInterpreter_OLEAUTO}
{$IFDEF JvInterpreter_DEBUG}
  if ObjCount <> 0 then
    Windows.MessageBox(0, PChar('Memory leak in JvInterpreter.pas'#13 +
      'ObjCount = ' + IntToStr(ObjCount)),
      'JvInterpreter Internal Error', MB_ICONERROR); 
{$ENDIF}
  GlobalJvInterpreterAdapter.Free;
end.


