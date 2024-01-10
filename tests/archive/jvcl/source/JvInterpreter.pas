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

Contributor(s): Dmitry Osinovsky, Peter Thornqvist, Olga Kobzar

Portions created by Dmitry Osinovsky and Olga Kobzar are
Copyright (C) 2003 ProgramBank Ltd.
All Rights Reserved.

Last Modified: 2003-04-10

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : JvInterpreterProgram and more..
description : JVCL Interpreter version 2

Known Issues:
   String fields in records binded from Delphi don't work
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
   - arrays as local and global variables. supports simple types (Integer,
     double, string, tdatetime, object).
     Added by Andrej Olejnik (olej@asset.sk);
   - type conversion with Integer, string, TObject,... keywords;
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
     return Integer, now it can return Boolean, if declared so;
  1.54:
   - new: in call to external function var-parameters are supported for
     Integer type;
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
Upcoming JVCL 3.00
   - major code cleanups
   - introduced data type system for variables and record fields initializations
   - interface (IInterface, IUnknown) method call support, see AddIntfGet
   - record declaration support
   - arrays of records, arrays of arrays
   - dynamic arrays
   - variant array support
   - arrays as parameters to Delphi procedures (sorry, no support for arrays
     as procedure parameters)
   - fixed record bugs with Delphi 6
   - fixed OLE bugs
}

{.$DEFINE JvInterpreter_DEBUG}

{$IFDEF COMPLIB_VCL}
{$DEFINE JvInterpreter_OLEAUTO}
{$ENDIF COMPLIB_VCL}

unit JvInterpreter;

interface

uses
  SysUtils, Classes, Math,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
{$IFDEF COMPILER6_UP}
  Variants,
{$ENDIF COMPILER6_UP}
  JvInterpreterParser, JvComponent;

const
  // (rom) renamed to longer names
  { max arguments to functions - small values increase performance }
  cJvInterpreterMaxArgs = 32;

  { max fields allowed in records }
  cJvInterpreterMaxRecFields = 32;

  // (rom) added
  cJvInterpreterStackMax = 199;

type
  { argument definition }
  PValueArray = ^TValueArray;
  TValueArray = array[0..cJvInterpreterMaxArgs] of Variant;
  PTypeArray = ^TTypeArray;
  TTypeArray = array[0..cJvInterpreterMaxArgs] of Word;
  PNameArray = ^TNameArray;
  TNameArray = array[0..cJvInterpreterMaxArgs] of string;

  TJvInterpreterArgs = class;
  IJvInterpreterDataType = interface;

  TJvInterpreterGetValue = procedure(Sender: TObject; Identifier: string; var Value: Variant;
    Args: TJvInterpreterArgs; var Done: Boolean) of object;
  TJvInterpreterSetValue = procedure(Sender: TObject; Identifier: string;
    const Value: Variant; Args: TJvInterpreterArgs; var Done: Boolean) of object;
  TJvInterpreterGetUnitSource = procedure(UnitName: string; var Source: string;
    var Done: Boolean) of object;

  TJvInterpreterAdapterGetValue = procedure(var Value: Variant; Args: TJvInterpreterArgs);
  TJvInterpreterAdapterSetValue = procedure(const Value: Variant; Args: TJvInterpreterArgs);
  TJvInterpreterAdapterNewRecord = procedure(var Value: Pointer);
  TJvInterpreterAdapterDisposeRecord = procedure(const Value: Pointer);
  TJvInterpreterAdapterCopyRecord = procedure(var Dest: Pointer; const Source: Pointer);

  POpenArray = ^TOpenArray;
  TOpenArray = array[0..cJvInterpreterMaxArgs] of TVarRec;

  TJvInterpreterRecField = record
    Identifier: string;
    Offset: Integer;
    Typ: Word;
    DataType: IJvInterpreterDataType;
  end;

  TJvInterpreterArgs = class(TObject)
  private
    VarNames: TNameArray;
    HasVars: Boolean;
  public
    Identifier: string;
    Count: Integer;
    Types: TTypeArray;
    Values: TValueArray;
    Names: TNameArray;
    HasResult: Boolean; { = False, if result not needed - used by calls
                          to ole automation servers }
    Assignment: Boolean; { internal }
    Obj: TObject;
    ObjTyp: Word; { varObject, varClass, varUnknown }
    ObjRefHolder: Variant; { if ObjType is varDispatch or varUnknown,
                              then we need to hold a reference to it }

    Indexed: Boolean; // if True then Args contain Indexes to Identifier
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
    OAV: PValueArray; { open array values }
  public
    { open array parameter support }
    OA: POpenArray; { open array }
    OAS: Integer; { open array size }
  end;

  { function descriptor }
  TJvInterpreterFunDesc = class(TObject)
  private
    FUnitName: string;
    FIdentifier: string;
    FClassIdentifier: string; { class name, if function declared as
                                TClassIdentifier.Identifier}
    FParamCount: Integer; { - 1..cJvInterpreterMaxArgs }
    FParamTypes: TTypeArray;
    FParamNames: TNameArray;
    FResTyp: Word;
    FResDataType: IJvInterpreterDataType;
    FPosBeg: Integer; { position in source }
    FPosEnd: Integer;
    function GetParamName(Index: Integer): string;
    function GetParamType(Index: Integer): Word;
  public
    property UnitName: string read FUnitName;
    property Identifier: string read FIdentifier;
    property ClassIdentifier: string read FClassIdentifier;
    property ParamCount: Integer read FParamCount;
    property ParamTypes[Index: Integer]: Word read GetParamType;
    property ParamNames[Index: Integer]: string read GetParamName;
    property ResTyp: Word read FResTyp;
    property ResDataType: IJvInterpreterDataType read FResDataType;
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
    FArgs: TJvInterpreterArgs;
    function GetArgs: TJvInterpreterArgs;
  protected
    constructor Create(AOwner: TJvInterpreterExpression; AInstance: TObject;
      AUnitName, AFunName: string); virtual;
    function CallFunction(Args: TJvInterpreterArgs; Params: array of Variant): Variant;
    property Args: TJvInterpreterArgs read GetArgs;
  public
    destructor Destroy; override;
  end;

  TJvInterpreterEventClass = class of TJvInterpreterEvent;

  { variable holder }
  TJvInterpreterVar = class(TObject)
  public
    UnitName: string;
    Identifier: string;
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
    procedure Clear;
{$IFDEF COMPILER35_UP} override;
{$ENDIF}
    procedure AddVar(UnitName, Identifier, Typ: string; VTyp: Word;
      const Value: Variant; DataType: IJvInterpreterDataType);
    function FindVar(const UnitName, Identifier: string): TJvInterpreterVar;
    procedure DeleteVar(const UnitName, Identifier: string);
    function GetValue(Identifier: string; var Value: Variant; Args: TJvInterpreterArgs)
      : Boolean;
    function SetValue(Identifier: string; const Value: Variant; Args: TJvInterpreterArgs)
      : Boolean;
  end;
 { notes about TJvInterpreterVarList implementation:
   - list must allow to contain more than one Var with same names;
   - FindVar must return last added Var with given name;
   - DeleteVar must delete last added Var with given name; }

  TJvInterpreterIdentifier = class(TObject)
  public
    UnitName: string;
    Identifier: string;
    Data: Pointer; // provided by user when call to adapter's addxxx methods
  end;

  TJvInterpreterIdentifierList = class(TList)
  private
    FDuplicates: TDuplicates;
  public
    function IndexOf(const UnitName, Identifier: string): TJvInterpreterIdentifier;
    function Find(const Identifier: string; var Index: Integer): Boolean;
    procedure Sort(Compare: TListSortCompare = nil); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  TJvInterpreterMethodList = class(TJvInterpreterIdentifierList)
  public
    procedure Sort(Compare: TListSortCompare = nil); override;
  end;

  IJvInterpreterDataType = interface
    ['{8C5E4071-65AB-11D7-B235-00A0D2043DC7}']
    procedure Init(var V: Variant);
    function GetTyp: Word;
  end;

  TCallConvention = set of (ccFastCall, ccStdCall, ccCDecl, ccDynamic,
    ccVirtual, ccClass);

  { TJvInterpreterAdapter - route JvInterpreter calls to Delphi functions }
  TJvInterpreterAdapter = class(TObject)
  private
    FOwner: TJvInterpreterExpression;
    FSrcUnitList: TJvInterpreterIdentifierList; // JvInterpreter-units sources
    FExtUnitList: TJvInterpreterIdentifierList; // internal units; like "system" in delphi
    FGetList: TJvInterpreterIdentifierList; // methods
    FSetList: TJvInterpreterIdentifierList; // write properties
    FIGetList: TJvInterpreterIdentifierList; // read indexed properties
    FISetList: TJvInterpreterIdentifierList; // write indexed properties
    FIDGetList: TJvInterpreterIdentifierList; // read default indexed properties
    FIDSetList: TJvInterpreterIdentifierList; // write default indexed properties
    FIntfGetList: TJvInterpreterIdentifierList;  // interface methods
    FDGetList: TJvInterpreterIdentifierList; // direct get list
    FClassList: TJvInterpreterIdentifierList; // delphi classes
    FConstList: TJvInterpreterIdentifierList; // delphi consts
    FFunList: TJvInterpreterIdentifierList; // functions, procedures
    FRecList: TJvInterpreterIdentifierList; // records
    FRecGetList: TJvInterpreterIdentifierList; // read record field
    FRecSetList: TJvInterpreterIdentifierList; // write record field
    FOnGetList: TJvInterpreterIdentifierList; // chain
    FOnSetList: TJvInterpreterIdentifierList; // chain
    FSrcFunList: TJvInterpreterIdentifierList; // functions, procedures in JvInterpreter-source
    FExtFunList: TJvInterpreterIdentifierList;
    FEventHandlerList: TJvInterpreterIdentifierList;
    FEventList: TJvInterpreterIdentifierList;
    FSrcVarList: TJvInterpreterVarList; // variables, constants in JvInterpreter-source
    FSrcClassList: TJvInterpreterIdentifierList; // JvInterpreter-source classes
    FSorted: Boolean;
    procedure CheckArgs(var Args: TJvInterpreterArgs; ParamCount: Integer;
      var ParamTypes: TTypeArray);
    function GetRec(RecordType: string): TObject;
{$IFDEF JvInterpreter_OLEAUTO}
    function DispatchCall(Identifier: string; var Value: Variant;
      Args: TJvInterpreterArgs; Get: Boolean): Boolean; stdcall;
{$ENDIF JvInterpreter_OLEAUTO}
    function GetValueRTTI(Identifier: string; var Value: Variant;
      Args: TJvInterpreterArgs): Boolean;
    function SetValueRTTI(Identifier: string; const Value: Variant;
      Args: TJvInterpreterArgs): Boolean;
  protected
    procedure CheckAction(Expression: TJvInterpreterExpression; Args: TJvInterpreterArgs;
      Data: Pointer); virtual;
    function GetValue(Expression: TJvInterpreterExpression; Identifier: string;
      var Value: Variant; Args: TJvInterpreterArgs): Boolean; virtual;
    function SetValue(Expression: TJvInterpreterExpression; Identifier: string;
      const Value: Variant; Args: TJvInterpreterArgs): Boolean; virtual;
    function GetElement(Expression: TJvInterpreterExpression; const Variable: Variant;
      var Value: Variant; var Args: TJvInterpreterArgs): Boolean; virtual;
    function SetElement(Expression: TJvInterpreterExpression; var Variable: Variant;
      const Value: Variant; var Args: TJvInterpreterArgs): Boolean; virtual;
    function NewRecord(const RecordType: string; var Value: Variant): Boolean; virtual;
    function FindFunDesc(const UnitName: string; const Identifier: string): TJvInterpreterFunDesc; virtual;
    procedure CurUnitChanged(NewUnitName: string; var Source: string); virtual;
    function UnitExists(const Identifier: string): Boolean; virtual;
    function IsEvent(Obj: TObject; const Identifier: string): Boolean; virtual;
    function NewEvent(const UnitName: string; const FunName, EventType: string;
      AOwner: TJvInterpreterExpression; AObject: TObject): TSimpleEvent; virtual;
    procedure ClearSource; dynamic;
    procedure ClearNonSource; dynamic;
    procedure Sort; dynamic;
  protected
    { for internal use }
    procedure AddSrcClass(JvInterpreterSrcClass: TJvInterpreterIdentifier); virtual;
    function GetSrcClass(Identifier: string): TJvInterpreterIdentifier; virtual;
  public
    constructor Create(AOwner: TJvInterpreterExpression);
    destructor Destroy; override;

    function SetRecord(var Value: Variant): Boolean; virtual;
    procedure Clear; dynamic;
    procedure Assign(Source: TJvInterpreterAdapter); dynamic;
    procedure AddSrcUnit(Identifier: string; Source: string; UsesList: string);
      dynamic;
    procedure AddSrcUnitEx(Identifier: string; Source: string; UsesList: string;
      Data: Pointer); dynamic;
    procedure AddExtUnit(Identifier: string); dynamic;
    procedure AddExtUnitEx(Identifier: string; Data: Pointer); dynamic;
    procedure AddClass(UnitName: string; AClassType: TClass; Identifier: string);
      dynamic;
    procedure AddClassEx(UnitName: string; AClassType: TClass; Identifier: string;
      Data: Pointer); dynamic;
    procedure AddIntfGet(IID: TGUID; Identifier: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word);
    procedure AddIntfGetEx(IID: TGUID; Identifier: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word; Data: Pointer);
    procedure AddGet(AClassType: TClass; Identifier: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word); dynamic;
    procedure AddGetEx(AClassType: TClass; Identifier: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word; Data: Pointer); dynamic;
    procedure AddSet(AClassType: TClass; Identifier: string;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word); dynamic;
    procedure AddSetEx(AClassType: TClass; Identifier: string;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word; Data: Pointer); dynamic;
    procedure AddIGet(AClassType: TClass; Identifier: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word); dynamic;
    procedure AddIGetEx(AClassType: TClass; Identifier: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word; Data: Pointer); dynamic;
    procedure AddISet(AClassType: TClass; Identifier: string;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word); dynamic;
    procedure AddISetEx(AClassType: TClass; Identifier: string;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word; Data: Pointer); dynamic;
    procedure AddIDGet(AClassType: TClass;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word); dynamic;
    procedure AddIDGetEx(AClassType: TClass;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word; Data: Pointer); dynamic;
    procedure AddIDSet(AClassType: TClass;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word); dynamic;
    procedure AddIDSetEx(AClassType: TClass;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word; Data: Pointer); dynamic;
    procedure AddFun(UnitName: string; Identifier: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word); dynamic;
    procedure AddFunEx(UnitName: string; Identifier: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word; Data: Pointer); dynamic;
    { function AddDGet under construction - don't use it }
    procedure AddDGet(AClassType: TClass; Identifier: string;
      GetFunc: Pointer; ParamCount: Integer; ParamTypes: array of Word;
      ResTyp: Word; CallConvention: TCallConvention); dynamic;
    procedure AddDGetEx(AClassType: TClass; Identifier: string;
      GetFunc: Pointer; ParamCount: Integer; ParamTypes: array of Word;
      ResTyp: Word; CallConvention: TCallConvention; Data: Pointer); dynamic;
    procedure AddRec(UnitName: string; Identifier: string; RecordSize: Integer;
      Fields: array of TJvInterpreterRecField; CreateFunc: TJvInterpreterAdapterNewRecord;
      DestroyFunc: TJvInterpreterAdapterDisposeRecord;
      CopyFunc: TJvInterpreterAdapterCopyRecord); dynamic;
    procedure AddRecEx(UnitName: string; Identifier: string; RecordSize: Integer;
      Fields: array of TJvInterpreterRecField; CreateFunc: TJvInterpreterAdapterNewRecord;
      DestroyFunc: TJvInterpreterAdapterDisposeRecord; CopyFunc: TJvInterpreterAdapterCopyRecord;
      Data: Pointer); dynamic;
    procedure AddRecGet(UnitName: string; RecordType: string; Identifier: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word); dynamic;
    procedure AddRecGetEx(UnitName: string; RecordType: string; Identifier: string;
      GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word; Data: Pointer); dynamic;
    procedure AddRecSet(UnitName: string; RecordType: string; Identifier: string;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word); dynamic;
    procedure AddRecSetEx(UnitName: string; RecordType: string; Identifier: string;
      SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer;
      ParamTypes: array of Word; Data: Pointer); dynamic;
    procedure AddConst(UnitName: string; Identifier: string; Value: Variant); dynamic;
    procedure AddConstEx(AUnitName: string; AIdentifier: string; AValue: Variant;
      AData: Pointer); dynamic;
    procedure AddExtFun(UnitName: string; Identifier: string; DllInstance: HINST;
      DllName: string; FunName: string; FunIndex: Integer; ParamCount: Integer;
      ParamTypes: array of Word; ResTyp: Word); dynamic;
    procedure AddExtFunEx(AUnitName: string; AIdentifier: string; ADllInstance: HINST;
      ADllName: string; AFunName: string; AFunIndex: Integer; AParamCount: Integer;
      AParamTypes: array of Word; AResTyp: Word; AData: Pointer); dynamic;
    procedure AddSrcFun(UnitName: string; Identifier: string;
      PosBeg, PosEnd: Integer; ParamCount: Integer; ParamTypes: array of Word;
      ParamNames: array of string; ResTyp: Word;
      AResDataType: IJvInterpreterDataType;
      Data: Pointer); dynamic;
    procedure AddSrcFunEx(AUnitName: string; AIdentifier: string;
      APosBeg, APosEnd: Integer; AParamCount: Integer; AParamTypes: array of Word;
      AParamNames: array of string; AResTyp: Word;
      AResDataType: IJvInterpreterDataType;
      AData: Pointer); dynamic;
    procedure AddHandler(UnitName: string; Identifier: string;
      EventClass: TJvInterpreterEventClass; Code: Pointer); dynamic;
    procedure AddHandlerEx(AUnitName: string; AIdentifier: string;
      AEventClass: TJvInterpreterEventClass; ACode: Pointer; AData: Pointer); dynamic;
    procedure AddEvent(UnitName: string; AClassType: TClass;
      Identifier: string); dynamic;
    procedure AddEventEx(AUnitName: string; AClassType: TClass;
      AIdentifier: string; AData: Pointer); dynamic;
    procedure AddSrcVar(UnitName: string; Identifier, Typ: string; VTyp: Word;
      const Value: Variant; DataType: IJvInterpreterDataType); dynamic;
    procedure AddOnGet(Method: TJvInterpreterGetValue); dynamic;
    procedure AddOnSet(Method: TJvInterpreterSetValue); dynamic;
  end;

  TStackPtr = -1..cJvInterpreterStackMax;

  { Expression evaluator }
  TJvInterpreterExpression = class(TJvComponent)
  private
    Parser: TJvInterpreterParser;
    FVResult: Variant;
    ExpStack: array[0..cJvInterpreterStackMax] of Variant;
    ExpStackPtr: TStackPtr;
    Token1: Variant;
    FBacked: Boolean;
    TTyp1: TTokenTyp;
    TokenStr1: string;
    PrevTTyp: TTokenTyp;
    AllowAssignment: Boolean;
    FArgs: TJvInterpreterArgs; { data }
    Args: TJvInterpreterArgs; { pointer to current }
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
      Args: TJvInterpreterArgs; Params: array of Variant): Variant; virtual; abstract;
    function CallFunctionEx(Instance: TObject; const UnitName: string;
      const FunName: string; Args: TJvInterpreterArgs; Params: array of Variant): Variant; virtual; abstract;
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
    function GetValue(Identifier: string; var Value: Variant;
      var Args: TJvInterpreterArgs): Boolean; virtual;
    function SetValue(Identifier: string; const Value: Variant;
      var Args: TJvInterpreterArgs): Boolean; virtual;
    function GetElement(const Variable: Variant; var Value: Variant;
      var Args: TJvInterpreterArgs): Boolean; virtual;
    function SetElement(var Variable: Variant; const Value: Variant;
      var Args: TJvInterpreterArgs): Boolean; virtual;
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


  TJvInterpreterAddVarFunc = procedure(UnitName: string;
    Identifier, Typ: string; VTyp: Word; const Value: Variant; ADataType: IJvInterpreterDataType) of object;

  { Function executor }
  TJvInterpreterFunction = class(TJvInterpreterExpression)
  private
    FCurUnitName: string;
    FCurInstance: TObject;
    FBreak, FContinue, FExit: Boolean;
    FunStack: TList;
    FunContext: Pointer; { PFunContext }
    SS: TStrings;
    StateStack: array[0..cJvInterpreterStackMax] of TParserState;
    StateStackPtr: TStackPtr;
    FEventList: TList;
    function GetLocalVars: TJvInterpreterVarList;
    function GetFunStackCount: Integer;
    function GetDebugPointerToGlobalVars: TJvInterpreterVarList;
    function GetDebugPointerToFunStack: Pointer;
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
    procedure SkipIdentifier1;
    procedure FindToken1(TTyp1: TTokenTyp);
    procedure Var1(AddVarFunc: TJvInterpreterAddVarFunc);
    procedure Const1(AddVarFunc: TJvInterpreterAddVarFunc);
    procedure Identifier1;
    procedure Begin1;
    procedure If1;
    procedure While1;
    procedure Repeat1;
    procedure For1;
    procedure Case1;
    procedure Try1;
    procedure Raise1;

    function ParseDataType: IJvInterpreterDataType;
    function NewEvent(const UnitName: string; const FunName, EventType: string;
      Instance: TObject): TSimpleEvent;
    procedure InternalSetValue(const Identifier: string);
    function GetValue(Identifier: string; var Value: Variant;
      var Args: TJvInterpreterArgs): Boolean; override;
    function SetValue(Identifier: string; const Value: Variant;
      var Args: TJvInterpreterArgs): Boolean; override;
    property LocalVars: TJvInterpreterVarList read GetLocalVars;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run; override;
    property CurUnitName: string read FCurUnitName;
    property CurInstance: TObject read FCurInstance;
    property FunStackCount: Integer read GetFunStackCount;
    property DebugPointerToFunStack: Pointer read GetDebugPointerToFunStack;
    property DebugPointerToGlobalVars: TJvInterpreterVarList read GetDebugPointerToGlobalVars;
  end;

  TUnitSection =
    (usUnknown, usInterface, usImplementation, usInitialization, usFinalization);

  { Unit executor }
  TJvInterpreterUnit = class(TJvInterpreterFunction)
  private
    FClearUnits: Boolean;
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
    procedure Class1(const Identifier: string);
    function GetValue(Identifier: string; var Value: Variant;
      var Args: TJvInterpreterArgs): Boolean; override;
    function SetValue(Identifier: string; const Value: Variant;
      var Args: TJvInterpreterArgs): Boolean; override;
    function GetUnitSource(UnitName: string; var Source: string): Boolean; dynamic;
    procedure ExecFunction(Fun: TJvInterpreterFunDesc);
    procedure SourceChanged; override;
    procedure Record1(const Identifier: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run; override;
    procedure DeclareExternalFunction(const Declaration: string);
    procedure Compile;
    function CallFunction(const FunName: string; Args: TJvInterpreterArgs;
      Params: array of Variant): Variant; override;
    function CallFunctionEx(Instance: TObject; const UnitName: string;
      const FunName: string; Args: TJvInterpreterArgs;
      Params: array of Variant): Variant; override;
    function FunctionExists(const UnitName: string;
      const FunName: string): Boolean;
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

{$IFDEF COMPILER6_UP}
  TJvSimpleVariantType = class(TCustomVariantType)
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
  end;

  TJvRecordVariantType = class(TJvSimpleVariantType);
  TJvObjectVariantType = class(TJvSimpleVariantType);
  TJvClassVariantType = class(TJvSimpleVariantType);
  TJvPointerVariantType = class(TJvSimpleVariantType);
  TJvSetVariantType = class(TJvSimpleVariantType);
  TJvArrayVariantType = class(TJvSimpleVariantType);
{$ENDIF COMPILER6_UP}

  EJvInterpreterError = class(Exception)
  private
    FExceptionPos: Boolean;
    FErrCode: Integer;
    FErrPos: Integer;
    FErrName: string;
    FErrName2: string;
    FErrUnitName: string;
    FErrLine: Integer;
    FMessage1: string;
  public
    constructor Create(const AErrCode: Integer; const AErrPos: Integer;
      const AErrName, AErrName2: string);
    procedure Assign(E: Exception);
    procedure Clear;
    property ErrCode: Integer read FErrCode;
    property ErrPos: Integer read FErrPos;
    property ErrName: string read FErrName;
    property ErrName2: string read FErrName2;
    property ErrUnitName: string read FErrUnitName;
    property ErrLine: Integer read FErrLine;
    property Message1: string read FMessage1;
  end;

 {Error raising routines}
procedure JvInterpreterError(const AErrCode: Integer; const AErrPos: Integer);
procedure JvInterpreterErrorN(const AErrCode: Integer; const AErrPos: Integer;
  const AErrName: string);
procedure JvInterpreterErrorN2(const AErrCode: Integer; const AErrPos: Integer;
  const AErrName1, AErrName2: string);

{Utilities functions}
//function LoadStr2(const ResID: Integer): string;

{ RFD - RecordFieldDefinition - return record needed for TJvInterpreterAdapter.AddRec
  Fields parameter }
function RFD(Identifier: string; Offset: Integer; Typ: Word): TJvInterpreterRecField;

{ raise error ieNotImplemented }
procedure NotImplemented(Message: string);

{ clear list of TObject }
procedure ClearList(List: TList);


{ additional variant types - TVarData.VType }

{$IFDEF COMPILER6_UP}
  function varRecord: TVarType;
  function varObject: TVarType;
  function varClass: TVarType;
  function varPointer: TVarType;
  function varSet: TVarType;
  function varArray: TVarType;
{$ELSE}
const
  varRecord = $0015;
  varObject = $0010;
  varClass = $0011;
  varPointer = $0012;
  varSet = $0013;
  varArray = $0014;
{$ENDIF COMPILER6_UP}


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

{ S2V - converts Integer to set and put it into variant }
function S2V(const I: Integer): Variant;

{ V2S - give a set from variant and converts it to Integer }
function V2S(V: Variant): Integer;

procedure V2OA(V: Variant; var OA: TOpenArray; var OAValues: TValueArray;
  var Size: Integer);

function TypeName2VarTyp(TypeName: string): Word;

{ copy variant variable with all JvInterpreter variant extension }
procedure JvInterpreterVarCopy(var Dest: Variant; const Source: Variant);

{ copy variant variable for assignment }
procedure JvInterpreterVarAssignment(var Dest: Variant; const Source: Variant);

function JvInterpreterVarAsType(const V: Variant; const VarType: Integer): Variant;

{ properly free var variable and set it content to empty }
procedure JvInterpreterVarFree(var V: Variant);

{ compare strings }
function Cmp(const S1, S2: string): Boolean;

{ For dynamic array support}
procedure JvInterpreterArraySetLength(AArray: Variant; ASize: integer);
function JvInterpreterArrayLength(const AArray: Variant): integer;
function JvInterpreterArrayLow(const AArray: Variant): integer;
function JvInterpreterArrayHigh(const AArray: Variant): integer;
procedure JvInterpreterArrayElementDelete(AArray: Variant; AElement: integer);
procedure JvInterpreterArrayElementInsert(AArray: Variant; AElement: integer; Value: Variant);

var
  GlobalJvInterpreterAdapter: TJvInterpreterAdapter = nil;

const
  prArgsNoCheck = -1;
  noInstance = HINST(0);
  RFDNull: TJvInterpreterRecField = (Identifier: ''; Offset: 0; Typ: 0);

  {JvInterpreter error codes}
  ieOk = 0; { Okay - no errors }
  ieUnknown = 1;
  ieInternal = 2;
  ieUserBreak = 3; { internal }
  ieRaise = 4; { internal }
  ieErrorPos = 5;
  ieExternal = 6; { non-interpreter error }
  ieAccessDenied = 7;
  ieExpressionStackOverflow = 8;

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
  ieDivisionByZero = ieRuntimeBase + 9; // protects us from low level math coprocessor exception

  { syntax errors (now run-timed) }
  ieSyntaxBase = 100;
  ieBadRemark = ieSyntaxBase + 1; { Bad remark - detected by parser }
  ieIdentifierExpected = ieSyntaxBase + 2;
  ieExpected = ieSyntaxBase + 3;
  ieUnknownIdentifier = ieSyntaxBase + 4;
  ieBooleanRequired = ieSyntaxBase + 5;
  ieClassRequired = ieSyntaxBase + 6;
  ieNotAllowedBeforeElse = ieSyntaxBase + 7;
  ieIntegerRequired = ieSyntaxBase + 8;
  ieROCRequired = ieSyntaxBase + 9;
  ieMissingOperator = ieSyntaxBase + 10;
  ieIdentifierRedeclared = ieSyntaxBase + 11;

  { array indexes }
  ieArrayBase = 170;
  ieArrayIndexOutOfBounds = ieArrayBase + 1;
  ieArrayTooManyParams = ieArrayBase + 2;
  ieArrayNotEnoughParams = ieArrayBase + 3;
  ieArrayBadDimension = ieArrayBase + 4;
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
  irIdentifier = 302;
  irDeclaration = 303;
  irEndOfFile = 304;
  irClass = 305;

implementation

uses
  TypInfo,
{$IFNDEF COMPILER3_UP}
  Ole2, { IUnknown in Delphi 2 }
{$ENDIF}
{$IFDEF JvInterpreter_OLEAUTO}
  OleConst,
{$IFDEF COMPILER3_UP}
  ActiveX, ComObj,
{$ELSE}
  OleAuto,
{$ENDIF COMPILER3_UP}
{$ENDIF JvInterpreter_OLEAUTO}
  JvInterpreterConst, JvUtils, JvStrUtil;

{$R JvInterpreter.res} { error messages }

{ internal structures }
type
  { Adapter classes - translates data from JvInterpreter calls to Delphi functions }
  TJvInterpreterSrcUnit = class(TJvInterpreterIdentifier)
  private
    Source: string;
    UsesList: TNameArray;
  end;

  TParamCount = -1..cJvInterpreterMaxArgs;

  TJvInterpreterMethod = class(TJvInterpreterIdentifier)
  private
    FClassType: TClass;
    ParamCount: TParamCount;
    ParamTypes: TTypeArray; { varInteger, varString, .. }
    ResTyp: Word; { varInteger, varString, .. }
    Func: Pointer; { TJvInterpreterAdapterGetValue or TJvInterpreterAdapterSetValue }
  end;

  TJvInterpreterIntfMethod = class(TJvInterpreterIdentifier)
  private
    IID: TGUID;
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

  TJvInterpreterClass = class(TJvInterpreterIdentifier)
  private
    FClassType: TClass;
  end;

  TJvInterpreterConst = class(TJvInterpreterIdentifier)
  private
    Value: Variant;
  end;

  TJvInterpreterRecFields = array[0..cJvInterpreterMaxRecFields] of TJvInterpreterRecField;

  TJvInterpreterRecord = class(TJvInterpreterIdentifier)
  private
    RecordSize: Integer; { SizeOf(Rec^) }
    FieldCount: Integer;
    Fields: TJvInterpreterRecFields;
    CreateFunc: TJvInterpreterAdapterNewRecord;
    DestroyFunc: TJvInterpreterAdapterDisposeRecord;
    CopyFunc: TJvInterpreterAdapterCopyRecord;

    procedure AddField(UnitName, Identifier, Typ: string; VTyp: Word;
      const Value: Variant; DataType: IJvInterpreterDataType);
    procedure NewRecord(var Value: Variant);
  end;

  TJvInterpreterRecMethod = class(TJvInterpreterIdentifier)
  private
    JvInterpreterRecord: TJvInterpreterRecord;
    ParamCount: TParamCount;
    ParamTypes: TTypeArray; { varInteger, varString and so one .. }
    ResTyp: Word; { varInteger, varString, .. }
    Func: Pointer; { TJvInterpreterAdapterGetValue or TJvInterpreterAdapterSetValue }
  end;

  TJvInterpreterRecHolder = class(TJvInterpreterIdentifier)
  private
    FRecordType: string;
    JvInterpreterRecord: TJvInterpreterRecord;
    Rec: Pointer; { data }
  public
    constructor Create(ARecordType: string; ARec: Pointer);
    destructor Destroy; override;

    property RecordType: string read FRecordType;
  end;

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
    DT : IJvInterpreterDataType;
    ElementSize: Integer; {size of element in bytes}
    Size: Integer; {number of elements in array}
    Memory: Pointer; {pointer to memory representation of array}
  end;


  TJvInterpreterRecordDataType = class(TInterfacedObject, IJvInterpreterDataType)
  private
    FRecordDesc: TJvInterpreterRecord;
  public
    constructor Create(ARecordDesc: TJvInterpreterRecord);
    procedure Init(var V: Variant);
    function GetTyp: Word;
  end;

  TJvInterpreterArrayDataType = class(TInterfacedObject, IJvInterpreterDataType)
  private
    FArrayBegin, FArrayEnd: TJvInterpreterArrayValues;
    FDimension: Integer;
    FArrayType: Integer;
    FDT: IJvInterpreterDataType;
  public
    constructor Create(AArrayBegin, AArrayEnd: TJvInterpreterArrayValues;
      ADimension: Integer; AArrayType: Integer; ADT: IJvInterpreterDataType);
    procedure Init(var V: Variant);
    function GetTyp: Word;
  end;

  TJvInterpreterSimpleDataType = class(TInterfacedObject, IJvInterpreterDataType)
  private
    FTyp: TVarType;
  public
    constructor Create(ATyp: TVarType);
    procedure Init(var V: Variant);
    function GetTyp: Word;
  end;

  PMethod = ^TMethod;

  { interpreter function }
  TJvInterpreterSrcFun = class(TJvInterpreterIdentifier)
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
    FunIndex: Integer;
    function CallDll(Args: TJvInterpreterArgs): Variant;
  end;

  { function context - stack }
  PFunContext = ^TFunContext;
  TFunContext = record
    PrevFunContext: PFunContext;
    LocalVars: TJvInterpreterVarList;
    Fun: TJvInterpreterSrcFun;
  end;

  TJvInterpreterEventDesc = class(TJvInterpreterIdentifier)
  private
    EventClass: TJvInterpreterEventClass;
    Code: Pointer;
  end;

{$IFDEF COMPILER2}

  { TJvStringStream  - reduced implementation from Delphi 3 classes.pas }
  TJvStringStream = class(TStream)
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
  DWORD = Longint;
  PBool = PBoolean;
{$ENDIF COMPLIB_CLX}

{$IFDEF JvInterpreter_DEBUG}
var
  ObjCount: Integer = 0;
{$ENDIF}

{$IFDEF COMPILER6_UP}
var
  VariantRecordInstance: TJvRecordVariantType;
  VariantObjectInstance: TJvObjectVariantType;
  VariantClassInstance: TJvClassVariantType;
  VariantPointerInstance: TJvPointerVariantType;
  VariantSetInstance: TJvSetVariantType;
  VariantArrayInstance: TJvArrayVariantType;

{ TJvSimpleVariantType }

procedure TJvSimpleVariantType.Clear(var V: TVarData);
begin
  SimplisticClear(V);
end;

procedure TJvSimpleVariantType.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: Boolean);
begin
  SimplisticCopy(Dest, Source, Indirect);
end;

function varRecord: TVarType;
begin
  Result := VariantRecordInstance.VarType
end;

function varObject: TVarType;
begin
  Result := VariantObjectInstance.VarType
end;

function varClass : TVarType;
begin
  Result := VariantClassInstance.VarType;
end;

function varPointer : TVarType;
begin
  Result := VariantPointerInstance.VarType;
end;

function varSet : TVarType;
begin
  Result := VariantSetInstance.VarType;
end;

function varArray : TVarType;
begin
  Result := VariantArrayInstance.VarType;
end;
{$ENDIF COMPILER6_UP}

//=== EJvInterpreterError ====================================================

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

procedure JvInterpreterError(const AErrCode: Integer; const AErrPos: Integer);
begin
  raise EJvInterpreterError.Create(AErrCode, AErrPos, '', '');
end;

procedure JvInterpreterErrorN(const AErrCode: Integer; const AErrPos: Integer;
  const AErrName: string);
begin
  raise EJvInterpreterError.Create(AErrCode, AErrPos, AErrName, '');
end;

procedure JvInterpreterErrorN2(const AErrCode: Integer; const AErrPos: Integer;
  const AErrName1, AErrName2: string);
begin
  raise EJvInterpreterError.Create(AErrCode, AErrPos, AErrName1, AErrName2);
end;

constructor EJvInterpreterError.Create(const AErrCode: Integer;
  const AErrPos: Integer; const AErrName, AErrName2: string);
var
 msg:String;
begin
  inherited Create(AErrName);
  FErrCode := AErrCode;
  FErrPos := AErrPos;
  FErrName := AErrName;
  FErrName2 := AErrName2;
  { function LoadStr don't work sometimes :-( }
  msg := Format(LoadStr2(ErrCode), [ErrName, ErrName2]);
  if (Length(msg)>0) then begin // not in RC file, use default text! -WP.
    Message := msg;
    FMessage1 := Message;
  end;
end;

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


function RFD(Identifier: string; Offset: Integer; Typ: Word): TJvInterpreterRecField;
begin
  Result.Identifier := Identifier;
  Result.Offset := Offset;
  Result.Typ := Typ;
end;

procedure NotImplemented(Message: string);
begin
  JvInterpreterErrorN(ieInternal, -1,
    Message + ' not implemented');
end;

//RWare: added check for "char", otherwise function with ref variable
//of type char causes AV, like KeyPress event handler

function Typ2Size(ATyp: Word): integer;
begin
  Result := 0;
  case ATyp of
    varInteger:
      begin
        Result := SizeOf(Integer);
      end;
    varDouble:
      begin
        Result := SizeOf(Double);
      end;
    varByte:
      begin
        Result := SizeOf(Byte);
      end;
    varSmallInt:
      begin
        Result := SizeOf(varSmallInt);
      end;
    varDate:
      begin
        Result := SizeOf(Double);
      end;
    varEmpty:
      begin
        Result := SizeOf(TVarData);
      end
  else if ATyp = varObject then
    Result := SizeOf(Integer);
  end;

end;

function TypeName2VarTyp(TypeName: string): Word;
begin
  if Cmp(TypeName, 'integer') or Cmp(TypeName, 'longint') or Cmp(TypeName, 'dword') then
    Result := varInteger
  else if Cmp(TypeName, 'word') or Cmp(TypeName, 'smallint') then
    Result := varSmallInt
  else if Cmp(TypeName, 'byte') then
    Result := varByte
  else if Cmp(TypeName, 'wordbool') or Cmp(TypeName, 'boolean') or Cmp(TypeName, 'bool') then
    Result := varBoolean
  else if Cmp(TypeName, 'string') or Cmp(TypeName, 'PChar') or
    Cmp(TypeName, 'ANSIString') or Cmp(TypeName, 'ShortString') or
    Cmp(TypeName, 'char') then {+RWare}
    Result := varString
  else if Cmp(TypeName, 'double') then
    Result := varDouble
  else if Cmp(TypeName, 'tdatetime') then
    Result := varDate
  else if Cmp(TypeName, 'tobject') then
    Result := varObject
  else
    Result := varEmpty;
end;

procedure ClearList(List: TList);
var
  i: Integer;
begin
  if not Assigned(List) then
    Exit;
  for i := 0 to List.Count - 1 do
    TObject(List[i]).Free;
  List.Clear;
end;

procedure ClearMethodList(List: TList);
var
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
    Dispose(PMethod(List[i]));
  List.Clear;
end;

//=== TJvStringStream ========================================================

{$IFNDEF COMPILER3_UP}

constructor TJvStringStream.Create(const AString: string);
begin
  inherited Create;
  FDataString := AString;
end;

function TJvStringStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Length(FDataString) - FPosition;
  if Result > Count then
    Result := Count;
  Move(PChar(@FDataString[FPosition + 1])^, Buffer, Result);
  Inc(FPosition, Result);
end;

function TJvStringStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
  SetLength(FDataString, (FPosition + Result));
  Move(Buffer, PChar(@FDataString[FPosition + 1])^, Result);
  Inc(FPosition, Result);
end;

function TJvStringStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning:
      FPosition := Offset;
    soFromCurrent:
      FPosition := FPosition + Offset;
    soFromEnd:
      FPosition := Length(FDataString) - Offset;
  end;
  Result := FPosition;
end;

procedure TJvStringStream.SetSize(NewSize: Longint);
begin
  SetLength(FDataString, NewSize);
  if FPosition > NewSize then
    FPosition := NewSize;
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

// (rom) JvUtil added to uses and funtions deleted

function Cmp(const S1, S2: string): Boolean;
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
end;

{************* Some code from RAStream unit **************}

procedure StringSaveToStream(Stream: TStream; S: string);
var
  L: Integer;
  P: PChar;
begin
  L := Length(S);
  Stream.WriteBuffer(L, SizeOf(L));
  P := PChar(S);
  Stream.WriteBuffer(P^, L);
end;

function StringLoadFromStream(Stream: TStream): string;
var
  L: Integer;
  P: PChar;
begin
  Stream.ReadBuffer(L, SizeOf(L));
  SetLength(Result, L);
  P := PChar(Result);
  Stream.ReadBuffer(P^, L);
end;

procedure IntSaveToStream(Stream: TStream; AInt: Integer);
begin
  Stream.WriteBuffer(AInt, SizeOf(AInt));
end;

function IntLoadFromStream(Stream: TStream): Integer;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

procedure WordSaveToStream(Stream: TStream; AWord: Word);
begin
  Stream.WriteBuffer(AWord, SizeOf(AWord));
end;

function WordLoadFromStream(Stream: TStream): Word;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

procedure ExtendedSaveToStream(Stream: TStream; AExt: Extended);
begin
  Stream.WriteBuffer(AExt, SizeOf(AExt));
end;

function ExtendedLoadFromStream(Stream: TStream): Extended;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

procedure BoolSaveToStream(Stream: TStream; ABool: Boolean);
var
  B: Integer;
begin
  B := Integer(ABool);
  Stream.WriteBuffer(B, SizeOf(B));
end;

function BoolLoadFromStream(Stream: TStream): Boolean;
var
  B: Integer;
begin
  Stream.ReadBuffer(B, SizeOf(B));
  Result := (B <> 0);
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
    if N = 0 then
      NameRefs[0] := P
    else
      NameRefs[NameCount - N] := P;
    repeat
      Ch := WideChar(Names[I]);
      WideNames[I] := Ch;
      Inc(I);
    until Char(Ch) = #0;
    Inc(N);
  until N = NameCount;
  { if Dispatch.GetIDsOfNames(GUID_NULL, @NameRefs, NameCount,
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
  GetIDsOfNames(IDispatch(Dispatch), Names, CallDesc^.NamedArgCount + 1, PDispIDList(@DispIDs[0]));
  if Result <> nil then
    VarClear(Result^);
{$IFDEF COMPILER3_UP}
  DispatchInvoke(IDispatch(Dispatch), CallDesc, PDispIDList(@DispIDs[0]), ParamTypes, Result);
{$ELSE}
  DispInvoke(Dispatch, CallDesc, PDispIDList(@DispIDs[0]), ParamTypes, Result);
{$ENDIF COMPILER3_UP}
end;

{################## from OleAuto unit ##################}
{$ENDIF JvInterpreter_OLEAUTO}

type
  TFunc = procedure; far;
  TiFunc = function: Integer; far;
  TfFunc = function: Boolean; far;
  TwFunc = function: Word; far;

function CallDllIns(Ins: HINST; FuncName: string; Args: TJvInterpreterArgs;
  ParamDesc: TTypeArray; ResTyp: Word): Variant;
var
  Func: TFunc;
  iFunc: TiFunc;
  fFunc: TfFunc;
  wFunc: TwFunc;
  i: Integer;
  Aint: Integer;
 // Abyte : Byte;
  Aword: Word;
  Apointer: Pointer;
  Str: string;
begin
  Result := Null;
  Func := GetProcAddress(Ins, PChar(FuncName));
  iFunc := @Func;
  fFunc := @Func;
  wFunc := @Func;
  if @Func <> nil then
  begin
    try
      for i := Args.Count - 1 downto 0 do { 'stdcall' call conversion }
      begin
        if (ParamDesc[i] and varByRef) = 0 then
          case ParamDesc[i] of
            varInteger, { ttByte,} varBoolean:
              begin
                Aint := Args.Values[i];
                asm push Aint
                end;
              end;
            varSmallInt:
              begin
                Aword := Word(Args.Values[i]);
                asm push Aword
                end;
              end;
            varString:
              begin
                Apointer := PChar(string(Args.Values[i]));
                asm push Apointer
                end;
              end;
          else
            JvInterpreterErrorN(ieDllInvalidArgument, -1, FuncName);
          end
        else
          case ParamDesc[i] and not varByRef of
            varInteger, { ttByte,} varBoolean:
              begin
                Apointer := @TVarData(Args.Values[i]).vInteger;
                asm push Apointer
                end;
              end;
            varSmallInt:
              begin
                Apointer := @TVarData(Args.Values[i]).vSmallInt;
                asm push Apointer
                end;
              end;
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
          Result := Boolean(Integer(fFunc));
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

function CallDll(DllName, FuncName: string; Args: TJvInterpreterArgs;
  ParamDesc: TTypeArray; ResTyp: Word): Variant;
var
  Ins: HMODULE;
  LastError: DWORD;
begin
  Result := False;
  Ins := LoadLibrary(PChar(DllName));
  if Ins = 0 then
    JvInterpreterErrorN(ieDllErrorLoadLibrary, -1, DllName);
  try
    Result := CallDllIns(Ins, FuncName, Args, ParamDesc, ResTyp);
    LastError := GetLastError;
  finally
    FreeLibrary(Ins);
  end;
  SetLastError(LastError);
end;

procedure ConvertParamTypes(InParams: array of Word; var OutParams: TTypeArray);
var
  i: Integer;
begin
  for i := Low(InParams) to High(InParams) do
    OutParams[i] := InParams[i];
end;

procedure ConvertParamNames(InParams: array of string;
  var OutParams: TNameArray);
var
  i: Integer;
begin
  for i := Low(InParams) to High(InParams) do
    OutParams[i] := InParams[i];
end;

{ ************************* Array support ************************* }

function GetArraySize(Dimension: Integer; BeginPos, EndPos: TJvInterpreterArrayValues): Integer;
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

function JvInterpreterArrayInit(const Dimension: Integer;
  const BeginPos, EndPos: TJvInterpreterArrayValues;
  const ItemType: Integer; DataType : IJvInterpreterDataType): PJvInterpreterArrayRec;
var
  PP: PJvInterpreterArrayRec;
  SS: TStringList;
  AA: Integer;
  ArraySize: Integer;
  i: integer;
begin
  if (Dimension < 1) or (Dimension > cJvInterpreterMaxArgs) then
    JvInterpreterError(ieArrayBadDimension, -1);
  for AA := 0 to Dimension - 1 do
  begin
    // For dynamic arrays BeginPos[AA] <= EndPos[AA]
    if (not (BeginPos[AA] <= EndPos[AA])) and
      (Dimension <> 1) and (BeginPos[AA] <> 0) and (EndPos[AA] <> - 1)then
      JvInterpreterError(ieArrayBadRange, -1);
  end;

  New(PP);
  PP^.BeginPos := BeginPos;
  PP^.EndPos := EndPos;
  PP^.ItemType := ItemType;
  PP^.DT := DataType;
  ArraySize := GetArraySize(Dimension, BeginPos, EndPos);
  PP^.Size := ArraySize;
  PP^.Dimension := Dimension;

  if ItemType <> varString then
    PP^.ElementSize := Typ2Size(ItemType)
  else
  begin
    PP^.ElementSize := 0;
    SS := TStringList.Create;
    for AA := 1 to ArraySize do
      SS.Add('');
    PP^.Memory := SS;
  end;

  if ItemType <> varString then
  begin
    GetMem(PP^.Memory, ArraySize * PP^.ElementSize);
    //ZeroMemory(PP^.Memory, ArraySize * PP^.ElementSize);
    FillChar(PP^.Memory^, ArraySize * PP^.ElementSize, 0);
    if ItemType = varEmpty then
      for i := 0 to ArraySize - 1 do
        PP^.DT.Init(Variant(PVarData(PChar(PP^.Memory) + i * PP^.ElementSize)^));
  end;
  Result := PP;
end;

{Free memory for array}

procedure JvInterpreterArrayFree(JvInterpreterArrayRec: PJvInterpreterArrayRec);
var
  i: integer;
  ArraySize: integer;
begin
  if not Assigned(JvInterpreterArrayRec) then
    Exit;
  ArraySize := GetArraySize(JvInterpreterArrayRec^.Dimension,
    JvInterpreterArrayRec^.BeginPos, JvInterpreterArrayRec^.EndPos);
  if JvInterpreterArrayRec^.ItemType <> varString then
  begin
    if JvInterpreterArrayRec^.ItemType = varEmpty then
      for i := 0 to ArraySize - 1 do
        JvInterpreterVarFree(Variant(PVarData(PChar(JvInterpreterArrayRec^.Memory) + i * JvInterpreterArrayRec^.ElementSize)^));
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
      end ;
    varEmpty:
      JvInterpreterVarAssignment(Variant(PVarData(PChar(JvInterpreterArrayRec^.Memory) +
        Offset * JvInterpreterArrayRec^.ElementSize)^), Value)
  else if JvInterpreterArrayRec^.ItemType = varObject then
      PInteger(Pointer(Integer(JvInterpreterArrayRec^.Memory) +
        (Offset * JvInterpreterArrayRec^.ElementSize)))^ := TVarData(Value).VInteger;
  end;
end;

{Get element for array}

function JvInterpreterArrayGetElement(Element: TJvInterpreterArrayValues;
  JvInterpreterArrayRec: PJvInterpreterArrayRec): Variant;
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
    varEmpty:
      JvInterpreterVarCopy(Result, Variant(PVarData(PChar(JvInterpreterArrayRec^.Memory) + Offset *
        JvInterpreterArrayRec^.ElementSize)^))
  else if JvInterpreterArrayRec^.ItemType = varObject then
      begin
        Result := Integer(Pointer(Integer(JvInterpreterArrayRec^.Memory) + ((Offset) *
          JvInterpreterArrayRec^.ElementSize))^);
        TVarData(Result).VType := varObject;
      end;
  end;
end;

{ For dynamic array support}
procedure JvInterpreterArraySetLength(AArray: Variant; ASize: integer);
var
  i: integer;
  OldSize: integer;
  ArrayRec: PJvInterpreterArrayRec;
begin
  ArrayRec := PJvInterpreterArrayRec(TVarData(AArray).VPointer);
  if ArrayRec^.Dimension > 1 then
    raise exception.Create('Sorry. Dynamic arrays support is made for one-dimensional arrays only.');
  OldSize := ArrayRec^.Size;
  if OldSize > ASize then
  begin
    for i := ASize to OldSize - 1 do
      if ArrayRec^.ItemType = varEmpty then
        JvInterpreterVarFree(Variant((PVarData(PChar(ArrayRec^.Memory) + i * ArrayRec^.ElementSize))^));
    ArrayRec^.EndPos[0] := ArrayRec^.EndPos[0] - (OldSize - ASize);
    ArrayRec^.Size := GetArraySize(1, ArrayRec^.BeginPos, ArrayRec^.EndPos);
    ReallocMem(ArrayRec^.Memory, ASize * ArrayRec^.ElementSize);
  end
  else if OldSize < ASize then
  begin
    ReallocMem(ArrayRec^.Memory, ASize * ArrayRec^.ElementSize);
    FillChar((PChar(ArrayRec^.Memory) + OldSize * ArrayRec^.ElementSize)^,
     (ASize - OldSize) * ArrayRec^.ElementSize, 0);
    for i := OldSize to ASize - 1 do
      if ArrayRec^.ItemType = varEmpty then
        ArrayRec^.DT.Init(Variant(Pointer(PChar(ArrayRec^.Memory) + i * ArrayRec^.ElementSize)^));
    ArrayRec^.EndPos[0] := ArrayRec^.EndPos[0] + (ASize - OldSize );
    ArrayRec^.Size := GetArraySize(ArrayRec^.Dimension, ArrayRec^.BeginPos, ArrayRec^.EndPos);
  end;
end;

function JvInterpreterArrayLength(const AArray: Variant): integer;
var
  ArrayRec: PJvInterpreterArrayRec;
begin
  ArrayRec := PJvInterpreterArrayRec(TVarData(AArray).VPointer);
  if ArrayRec^.Dimension > 1 then
    raise exception.Create('Sorry. For one-dimensional arrays only.');
  Result := ArrayRec^.Size;
end;

function JvInterpreterArrayLow(const AArray: Variant): integer;
var
  ArrayRec: PJvInterpreterArrayRec;
begin
  ArrayRec := PJvInterpreterArrayRec(TVarData(AArray).VPointer);
  if ArrayRec^.Dimension > 1 then
    raise exception.Create('Sorry. For one-dimensional arrays only.');
  Result := ArrayRec^.BeginPos[0];
end;

function JvInterpreterArrayHigh(const AArray: Variant): integer;
var
  ArrayRec: PJvInterpreterArrayRec;
begin
  ArrayRec := PJvInterpreterArrayRec(TVarData(AArray).VPointer);
  if ArrayRec^.Dimension > 1 then
    raise exception.Create('Sorry. For one-dimensional arrays only.');
  Result := ArrayRec^.EndPos[0];
end;

procedure JvInterpreterArrayElementDelete(AArray: Variant; AElement: integer);
var
  ArrayRec: PJvInterpreterArrayRec;
begin
  ArrayRec := PJvInterpreterArrayRec(TVarData(AArray).VPointer);
  if ArrayRec^.Dimension > 1 then
    raise exception.Create('Sorry. For one-dimensional arrays only.');
  if (AElement < ArrayRec^.BeginPos[0]) or (AElement > ArrayRec^.EndPos[0]) then
    JvInterpreterError(ieArrayIndexOutOfBounds, -1);
  ArrayRec^.EndPos[0] := ArrayRec^.EndPos[0] - 1;
  ArrayRec^.Size := GetArraySize(ArrayRec^.Dimension, ArrayRec^.BeginPos, ArrayRec^.EndPos);
  if ArrayRec^.ItemType = varEmpty then
    JvInterpreterVarFree(Variant(PVarData(PChar(ArrayRec^.Memory) +
      (AElement - ArrayRec^.BeginPos[0]) * ArrayRec^.ElementSize)^));
  Move((PChar(ArrayRec^.Memory) + (AElement - ArrayRec^.BeginPos[0] + 1) *ArrayRec^.ElementSize)^,
    (PChar(ArrayRec^.Memory) + (AElement - ArrayRec^.BeginPos[0]) *ArrayRec^.ElementSize)^,
    (ArrayRec^.EndPos[0]-AElement + 1) * ArrayRec^.ElementSize);
  ReallocMem(ArrayRec^.Memory, ArrayRec^.Size * ArrayRec^.ElementSize);

end;

procedure JvInterpreterArrayElementInsert(AArray: Variant; AElement: integer; Value: Variant);
var
  ArrayRec: PJvInterpreterArrayRec;
begin
  ArrayRec := PJvInterpreterArrayRec(TVarData(AArray).VPointer);
  if ArrayRec^.Dimension > 1 then
    raise exception.Create('Sorry.For one-dimensional arrays only.');
  if (AElement < ArrayRec^.BeginPos[0]) or (AElement > ArrayRec^.EndPos[0]) then
    JvInterpreterError(ieArrayIndexOutOfBounds, -1);
  ArrayRec^.EndPos[0] := ArrayRec^.EndPos[0] + 1;
  ArrayRec^.Size := GetArraySize(ArrayRec^.Dimension, ArrayRec^.BeginPos, ArrayRec^.EndPos);
  ReallocMem(ArrayRec^.Memory, ArrayRec^.Size * ArrayRec^.ElementSize);
  Move((PChar(ArrayRec^.Memory) + (AElement - ArrayRec^.BeginPos[0]) * ArrayRec^.ElementSize)^,
    (PChar(ArrayRec^.Memory) + (AElement - ArrayRec^.BeginPos[0] + 1) * ArrayRec^.ElementSize)^,
    (ArrayRec^.EndPos[0] - AElement) * ArrayRec^.ElementSize);
  if ArrayRec^.ItemType = varEmpty then
    ArrayRec^.DT.Init(Variant(PVarData(PChar(ArrayRec^.Memory) +
      (AElement - ArrayRec^.BeginPos[0]) * ArrayRec^.ElementSize)^));
  JvInterpreterVarAssignment(Variant(PVarData(PChar(ArrayRec^.Memory) +
    (AElement - ArrayRec^.BeginPos[0]) * ArrayRec^.ElementSize)^), Value);
end;

procedure V2OA(V: Variant; var OA: TOpenArray; var OAValues: TValueArray;
  var Size: Integer);
var
  i: Integer;
  ArrayRec: PJvInterpreterArrayRec;
  Element: TJvInterpreterArrayValues;
  ElementVariant: Variant;
begin
  if TVarData(V).VType = varArray then
  //JvInterpreterError(ieTypeMistmatch, -1);
  begin
    ArrayRec := PJvInterpreterArrayRec(TVarData(V).VPointer);
    if ArrayRec^.Dimension > 1 then
      raise exception.Create('Sorry.For one-dimensional arrays only.');
    Size := ArrayRec^.Size;
    for i := 0 to Size - 1 do
    begin
      Element[0] := i;
      ElementVariant := JvInterpreterArrayGetElement(Element, ArrayRec);
      case TVarData(ElementVariant).VType of
        varInteger, varSmallInt:
          begin
            OAValues[i] := ElementVariant;
            OA[i].vInteger := ElementVariant;
            OA[i].VType := vtInteger;
          end;
        varString, varOleStr:
          begin
            // OA[i].vPChar := PChar(string(V[i]));
            // OA[i].VType := vtPChar;
            OAValues[i] := ElementVariant;
            OA[i].vVariant := @OAValues[i];
            OA[i].VType := vtVariant;
          end;
        varBoolean:
          begin
            OAValues[i] := ElementVariant;
            OA[i].vBoolean := ElementVariant;
            OA[i].VType := vtBoolean;
          end;
        varDouble, varCurrency:
          begin
            OAValues[i] := ElementVariant;
            OA[i].vExtended := TVarData(ElementVariant).vPointer;
            OA[i].VType := vtExtended;
          end;
        else
          begin
            OAValues[i] := ElementVariant;
            OA[i].vVariant := @OAValues[i];
            OA[i].VType := vtVariant;
          end;
      end;
    end;
  end
  else
  begin
    Size := VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1;
    for i := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
    begin
      case TVarData(V[i]).VType of
        varInteger, varSmallInt:
          begin
            OAValues[i] := V[i];
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
            OAValues[i] := V[i];
            OA[i].vBoolean := V[i];
            OA[i].VType := vtBoolean;
          end;
        varDouble, varCurrency:
          begin
            OAValues[i] := V[i];
            OA[i].vExtended := TVarData(V[i]).vPointer;
            OA[i].VType := vtExtended;
          end;
        else
          begin
            OAValues[i] := V[i];
            OA[i].vVariant := @OAValues[i];
            OA[i].VType := vtVariant;
          end;
      end;
    end;
  end;

end;

{ ########################## Array support ########################## }

{ ************************ extended variants ************************ }

function JvInterpreterVarAsType(const V: Variant; const VarType: Integer): Variant;
begin
  if (TVarData(V).VType = varEmpty) or (TVarData(V).VType = varNull) then
  begin
    case VarType of
      varString, varOleStr:
        Result := '';
      varInteger, varSmallint, varByte:
        Result := 0;
      varBoolean:
        Result := False;
      varSingle, varDouble, varCurrency, varDate:
        Result := 0.0;
      varVariant:
        Result := Null;
    else
      Result := VarAsType(V, VarType);
    end;
  end
  else
  begin
    if TVarData(V).VType = varInteger then
    begin
      if (TVarData(V).VType = VarType) then
        Result := Integer(V = True)
      else
        Result := Integer(V)
    end
    else if TVarData(V).VType = varArray then
      begin
        TVarData(Result) := TVarData(V);
        TVarData(Result).VType := VarType;
      end
    else
      Result := VarAsType(V, VarType);
  end;
end;

procedure JvInterpreterVarAssignment(var Dest: Variant; const Source: Variant);
var
  i: integer;
  DestRecHolder: TJvInterpreterRecHolder;
  SourceRecHolder: TJvInterpreterRecHolder;
begin
  if (TVarData(Source).VType = varArray) then
    TVarData(Dest) := TVarData(Source)
  else if (TVarData(Source).VType = varRecord) then
  begin
    DestRecHolder := TJvInterpreterRecHolder(TVarData(Dest).VPointer);
    SourceRecHolder := TJvInterpreterRecHolder(TVarData(Source).VPointer);
    for i := 0 to SourceRecHolder.JvInterpreterRecord.FieldCount - 1 do
      if SourceRecHolder.JvInterpreterRecord.Fields[i].Typ = varEmpty then
        JvInterpreterVarAssignment(Variant(PVarData(PChar(DestRecHolder.Rec) +
          DestRecHolder.JvInterpreterRecord.Fields[i].Offset)^),
          Variant(PVarData(PChar(SourceRecHolder.Rec) +
          SourceRecHolder.JvInterpreterRecord.Fields[i].Offset)^))
      else
        Move((PChar(SourceRecHolder.Rec) +
          SourceRecHolder.JvInterpreterRecord.Fields[i].Offset)^,
          (PChar(DestRecHolder.Rec) +
          DestRecHolder.JvInterpreterRecord.Fields[i].Offset)^,
          Typ2Size(SourceRecHolder.JvInterpreterRecord.Fields[i].Typ));
  end
  else
    Dest := Source;
end;

procedure JvInterpreterVarCopy(var Dest: Variant; const Source: Variant);
begin
  if (TVarData(Source).VType = varArray)
    or (TVarData(Source).VType = varRecord) then
    TVarData(Dest) := TVarData(Source)
  else
    Dest := Source;
end;

procedure JvInterpreterVarFree(var V: Variant);
var
  TempType : TVarType;
begin
  TempType := TVarData(V).VType;
  if TempType = varArray then
    JvInterpreterArrayFree(PJvInterpreterArrayRec(TVarData(V).vPointer))
  else if TempType = varRecord then
    TJvInterpreterRecHolder(TVarData(V).VPointer).Free;
  {case TVarData(V).VType of
    varArray:
      JvInterpreterArrayFree(PJvInterpreterArrayRec(TVarData(V).vPointer));
    varRecord:
      TJvInterpreterRecHolder(TVarData(V).VPointer).Free;
  end;}
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

function Var2Type(V: Variant; const VarType: Integer): Variant;
begin
  if (TVarData(V).VType = varEmpty) or (TVarData(V).VType = varNull) then
  begin
    case VarType of
      varString, varOleStr:
        Result := '';
      varInteger, varSmallint, varByte:
        Result := 0;
      varBoolean:
        Result := False;
      varSingle, varDouble, varCurrency, varDate:
        Result := 0.0;
      varVariant:
        Result := Null;
    else
      Result := VarAsType(V, VarType);
    end;
  end
  else
    Result := VarAsType(V, VarType);
  if (VarType = varInteger) and (TVarData(V).VType = varBoolean) then
    Result := Integer(V = True);
end;
{ ######################## extended variants ######################## }

//=== TJvInterpreterVar ======================================================

destructor TJvInterpreterVar.Destroy;
begin
  JvInterpreterVarFree(Value);
  inherited Destroy;
end;

//=== TJvInterpreterVarList ==================================================

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

procedure TJvInterpreterVarList.AddVar(UnitName, Identifier, Typ: string; VTyp: Word;
  const Value: Variant; DataType: IJvInterpreterDataType);
var
  VarRec: TJvInterpreterVar;
begin
  if FindVar(UnitName, Identifier) <> nil then
    JvInterpreterErrorN(ieIdentifierRedeclared, -1, Identifier);
  VarRec := TJvInterpreterVar.Create;
  VarRec.Identifier := Identifier;
  VarRec.UnitName := UnitName;
  JvInterpreterVarCopy(VarRec.Value, Value);
  VarRec.Typ := Typ;
  VarRec.VTyp := VTyp;
  Insert(0, VarRec);
end;

function TJvInterpreterVarList.FindVar(const UnitName, Identifier: string): TJvInterpreterVar;
{ if UnitName = '', any unit allowed}
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := TJvInterpreterVar(Items[i]);
    if Cmp(Result.Identifier, Identifier) and
      (Cmp(Result.UnitName, UnitName) or (UnitName = '')) then
      Exit;
  end;
  Result := nil;
end;

procedure TJvInterpreterVarList.DeleteVar(const UnitName, Identifier: string);
var
  i: Integer;
  VarRec: TJvInterpreterVar;
begin
  for i := 0 to Count - 1 do
  begin
    VarRec := TJvInterpreterVar(Items[i]);
    if Cmp(VarRec.Identifier, Identifier) and
      (Cmp(VarRec.UnitName, UnitName) or (UnitName = '')) then
    begin
      JvInterpreterVarFree(VarRec.Value);
      VarRec.Free;
      Delete(i);
      Exit;
    end;
  end;
end;

function TJvInterpreterVarList.GetValue(Identifier: string; var Value: Variant;
  Args: TJvInterpreterArgs): Boolean;
var
  V: TJvInterpreterVar;
begin
  if (Args.Obj = nil) then
    V := FindVar('', Identifier)
  else if (Args.ObjTyp = varObject) and (Args.Obj is TJvInterpreterSrcUnit) then
    V := FindVar((Args.Obj as TJvInterpreterSrcUnit).Identifier, Identifier)
  else
    V := nil;
  Result := V <> nil;
  if Result then
    JvInterpreterVarCopy(Value, V.Value);
end;

(*
function TJvInterpreterVarList.SetValue(Identifier: string; const Value: Variant;
  Args: TJvInterpreterArgs): Boolean;
var
  V: TJvInterpreterVar;
begin
  V := FindVar('', Identifier);
  Result := (V <> nil) and (Args.Obj = nil);
  if Result then
    JvInterpreterVarAssignment(V.Value, Value);
end; { SetValue }
*)

function TJvInterpreterVarList.SetValue(Identifier: string; const Value: Variant;
  Args: TJvInterpreterArgs): Boolean;
var
  V: TJvInterpreterVar;
begin
  V := FindVar('', Identifier);
  Result := (V <> nil) and (Args.Obj = nil);
  if Result then
    { If 0, then it's probably an object }
    { If a Variant, then we don't care about typecasting }
    { We only want to typecast if the types are not the same, for speed }
    if (V.VTyp <> 0) and
      (V.VTyp <> varVariant) and
      (TVarData(Value).VType <> V.VTyp) then
    begin
      { Is it a passed-by-reference variable? }
      if V.VTyp and VarByRef > 0 then
      begin
        JvInterpreterVarAssignment(V.Value, JvInterpreterVarAsType(Value, V.VTyp and not VarByRef));
        V.VTyp := V.VTyp or VarByRef;
      end
      else
        try
        JvInterpreterVarAssignment(V.Value, JvInterpreterVarAsType(Value, V.VTyp))
          except
           on E:EVariantOverflowError do begin
            JvInterpreterVarAssignment(V.Value, NULL);
            JvInterpreterErrorN2(ieIntegerOverflow, -1, 'VariantOverflow', 'Variant Overflow Error');
          end;
          on E:ERangeError do begin
           JvInterpreterVarAssignment(V.Value, NULL);
           JvInterpreterErrorN2(ieIntegerOverflow, -1, 'RangeOverflow', 'Array or Subrange Overflow (ERangeError)' );
          end;
        end;
    end
    else
      JvInterpreterVarAssignment(V.Value, Value);
end;

//=== TJvInterpreterFunDesc ==================================================

function TJvInterpreterFunDesc.GetParamType(Index: Integer): Word;
begin
  Result := FParamTypes[Index];
end;

function TJvInterpreterFunDesc.GetParamName(Index: Integer): string;
begin
  Result := FParamNames[Index];
end;

//=== TJvInterpreterRecHolder ================================================

constructor TJvInterpreterRecHolder.Create(ARecordType: string; ARec: Pointer);
begin
  // (rom) added inherited Create
  inherited Create;
  Assert(ARecordType <> '');
  FRecordType := ARecordType;
  Rec := ARec;
{$IFDEF JvInterpreter_DEBUG}
  Inc(ObjCount);
{$ENDIF JvInterpreter_DEBUG}
end;

destructor TJvInterpreterRecHolder.Destroy;
var
  i: integer;
begin
  if Assigned(JvInterpreterRecord) then
  begin
    if Assigned(JvInterpreterRecord.DestroyFunc) then
      JvInterpreterRecord.DestroyFunc(Rec)
    else
    begin
      for i := 0 to JvInterpreterRecord.FieldCount-1 do
      begin
        if JvInterpreterRecord.Fields[i].Typ = varEmpty then
          JvInterpreterVarFree(Variant(PVarData(PChar(Rec) + JvInterpreterRecord.Fields[i].Offset)^));
      end;
      FreeMem(Rec, JvInterpreterRecord.RecordSize);
    end;
  end
  else
    JvInterpreterError(ieInternal, -1);
  inherited Destroy;
{$IFDEF JvInterpreter_DEBUG}
  Dec(ObjCount);
{$ENDIF JvInterpreter_DEBUG}
end;

//=== TJvInterpreterSrcFun ===================================================

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

//=== TJvInterpreterExtFun ===================================================

function TJvInterpreterExtFun.CallDll(Args: TJvInterpreterArgs): Variant;
begin
  if DllInstance > 0 then
    Result := JvInterpreter.CallDllIns(DllInstance, FunName, Args, FunDesc.FParamTypes,
      FunDesc.ResTyp)
  else
    Result := JvInterpreter.CallDll(DllName, FunName, Args, FunDesc.FParamTypes,
      FunDesc.ResTyp)
end;

//=== TJvInterpreterEvent ====================================================

constructor TJvInterpreterEvent.Create(AOwner: TJvInterpreterExpression; AInstance: TObject;
  AUnitName, AFunName: string);
begin
  // (rom) added inherited Create
  inherited Create;
  Owner := AOwner;
  Instance := AInstance;
  UnitName := AUnitName;
  FunName := AFunName;
{$IFDEF JvInterpreter_DEBUG}
  Inc(ObjCount);
{$ENDIF JvInterpreter_DEBUG}
end;

destructor TJvInterpreterEvent.Destroy;
begin
  FArgs.Free;
  inherited Destroy;
{$IFDEF JvInterpreter_DEBUG}
  Dec(ObjCount);
{$ENDIF JvInterpreter_DEBUG}
end;

function TJvInterpreterEvent.GetArgs: TJvInterpreterArgs;
begin
  if FArgs = nil then
    FArgs := TJvInterpreterArgs.Create;
  Result := FArgs;
end;

function TJvInterpreterEvent.CallFunction(Args: TJvInterpreterArgs;
  Params: array of Variant): Variant;
var
  i: Integer;
  NV: Variant;
begin
  if Args = nil then
    Args := Self.Args;
  Args.Clear;
  for i := Low(Params) to High(Params) do
  begin
    Args.Values[Args.Count] := Params[i];
    Inc(Args.Count);
  end;
  NV := Null;
  Result := Owner.CallFunctionEx(Instance, UnitName, FunName, Args, NV);
end;

//=== TJvInterpreterIdentifierList ============================================

function TJvInterpreterIdentifierList.Find(const Identifier: string;
  var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := AnsiStrIComp(PChar(TJvInterpreterIdentifier(List[I]).Identifier), PChar(Identifier));
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

procedure TJvInterpreterIdentifierList.Sort(Compare: TListSortCompare = nil);

  function SortIdentifier(Item1, Item2: Pointer): Integer;
  begin
    { function AnsiStrIComp about 30% faster when AnsiCompareText }
    { Result := AnsiCompareText(TJvInterpreterIdentifier(Item1).Identifier,
       TJvInterpreterIdentifier(Item2).Identifier); }
    Result := AnsiStrIComp(PChar(TJvInterpreterIdentifier(Item1).Identifier),
      PChar(TJvInterpreterIdentifier(Item2).Identifier));
  end;

begin
  if Assigned(Compare) then
    inherited Sort(Compare)
  else
    inherited Sort(TListSortCompare(@SortIdentifier))
end;

function TJvInterpreterIdentifierList.IndexOf(const UnitName, Identifier: string): TJvInterpreterIdentifier;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    Result := TJvInterpreterIdentifier(Items[i]);
    if Cmp(Result.Identifier, Identifier) and
      (Cmp(Result.UnitName, UnitName) or (UnitName = '')) then
      Exit;
  end;
  Result := nil;
end;

{************************ TJvInterpreterAdapter ************************}

//=== TJvInterpreterAdapter ==================================================

constructor TJvInterpreterAdapter.Create(AOwner: TJvInterpreterExpression);
begin
  // (rom) added inherited Create
  inherited Create;
  FOwner := AOwner;
  FSrcUnitList := TJvInterpreterIdentifierList.Create;
  FExtUnitList := TJvInterpreterIdentifierList.Create;
  FIntfGetList := TJvInterpreterIdentifierList.Create;
  FGetList     := TJvInterpreterMethodList.Create;
  FSetList     := TJvInterpreterMethodList.Create;
  FIGetList    := TJvInterpreterMethodList.Create;
  FISetList    := TJvInterpreterMethodList.Create;
  FIDGetList   := TJvInterpreterIdentifierList.Create;
  FIDSetList   := TJvInterpreterIdentifierList.Create;
  FDGetList    := TJvInterpreterIdentifierList.Create;
  FClassList   := TJvInterpreterIdentifierList.Create;
  FConstList   := TJvInterpreterIdentifierList.Create;
  FFunList     := TJvInterpreterMethodList.Create;
  FRecList     := TJvInterpreterIdentifierList.Create;
  FRecGetList  := TJvInterpreterIdentifierList.Create;
  FRecSetList  := TJvInterpreterIdentifierList.Create;
  FOnGetList   := TJvInterpreterIdentifierList.Create;
  FOnSetList   := TJvInterpreterIdentifierList.Create;
  FExtFunList  := TJvInterpreterIdentifierList.Create;
  FSrcFunList  := TJvInterpreterIdentifierList.Create;
  FEventHandlerList := TJvInterpreterIdentifierList.Create;
  FEventList        := TJvInterpreterIdentifierList.Create;
  FSrcVarList       := TJvInterpreterVarList.Create;
  FSrcClassList     := TJvInterpreterIdentifierList.Create;

  FIntfGetList.Duplicates := dupAccept;
  FGetList.Duplicates  := dupAccept;
  FSetList.Duplicates  := dupAccept;
  FIGetList.Duplicates := dupAccept;
  FISetList.Duplicates := dupAccept;
end;

destructor TJvInterpreterAdapter.Destroy;
begin
  Clear;
  FSrcUnitList.Free;
  FExtUnitList.Free;
  FIntfGetList.Free;
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

procedure TJvInterpreterAdapter.ClearSource;
begin
  ClearList(FSrcUnitList);
  ClearList(FSrcFunList);
  FSrcVarList.Clear;
  ClearList(FSrcClassList);
end;

procedure TJvInterpreterAdapter.ClearNonSource;
begin
  ClearList(FExtUnitList);
  ClearList(FIntfGetList);
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

procedure TJvInterpreterAdapter.Clear;
begin
  ClearSource;
  ClearNonSource;
end;

procedure TJvInterpreterAdapter.Assign(Source: TJvInterpreterAdapter);
var
  i: Integer;
begin
  if Source = Self then
    Exit;
  for i := 0 to Source.FGetList.Count - 1 do
    with TJvInterpreterMethod(Source.FGetList[i]) do
      AddGetEx(FClassType, Identifier, Func, ParamCount, ParamTypes, ResTyp, Data);
  for i := 0 to Source.FSetList.Count - 1 do
    with TJvInterpreterMethod(Source.FSetList[i]) do
      AddSetEx(FClassType, Identifier, Func, ParamCount, ParamTypes, Data);
  for i := 0 to Source.FIGetList.Count - 1 do
    with TJvInterpreterMethod(Source.FIGetList[i]) do
      AddIGetEx(FClassType, Identifier, Func, ParamCount, ParamTypes, ResTyp, Data);
  for i := 0 to Source.FISetList.Count - 1 do
    with TJvInterpreterMethod(Source.FISetList[i]) do
      AddISetEx(FClassType, Identifier, Func, ParamCount, ParamTypes, Data);
  for i := 0 to Source.FIDGetList.Count - 1 do
    with TJvInterpreterMethod(Source.FIDGetList[i]) do
      AddIDGetEx(FClassType, Func, ParamCount, ParamTypes, ResTyp, Data);
  for i := 0 to Source.FIDSetList.Count - 1 do
    with TJvInterpreterMethod(Source.FIDSetList[i]) do
      AddIDSetEx(FClassType, Func, ParamCount, ParamTypes, Data);
  for i := 0 to Source.FIntfGetList.Count - 1 do
    with TJvInterpreterIntfMethod(Source.FIntfGetList[i]) do
       AddIntfGetEx(IID, Identifier, Func, ParamCount, ParamTypes, ResTyp, Data);
  for i := 0 to Source.FDGetList.Count - 1 do
    with TJvInterpreterDMethod(Source.FDGetList[i]) do
      AddDGetEx(FClassType, Identifier, Func, ParamCount, ParamTypes, ResTyp,
        CallConvention, Data);
  for i := 0 to Source.FFunList.Count - 1 do
    with TJvInterpreterMethod(Source.FFunList[i]) do
      AddFunEx(UnitName, Identifier, Func, ParamCount, ParamTypes, ResTyp, Data);
  for i := 0 to Source.FExtUnitList.Count - 1 do
    with TJvInterpreterIdentifier(Source.FExtUnitList[i]) do
      AddExtUnitEx(Identifier, Data);
  for i := 0 to Source.FClassList.Count - 1 do
    with TJvInterpreterClass(Source.FClassList[i]) do
      AddClassEx(UnitName, FClassType, Identifier, Data);
  for i := 0 to Source.FConstList.Count - 1 do
    with TJvInterpreterConst(Source.FConstList[i]) do
      AddConstEx(UnitName, Identifier, Value, Data);
  for i := 0 to Source.FRecList.Count - 1 do
    with TJvInterpreterRecord(Source.FRecList[i]) do
      AddRecEx(UnitName, Identifier, RecordSize, Fields, CreateFunc,
        DestroyFunc, CopyFunc, Data);
  for i := 0 to Source.FRecGetList.Count - 1 do
    with TJvInterpreterRecMethod(Source.FRecGetList[i]) do
      AddRecGetEx(UnitName, JvInterpreterRecord.Identifier, Identifier, Func, ParamCount,
        ParamTypes, ResTyp, Data);
  for i := 0 to Source.FRecSetList.Count - 1 do
    with TJvInterpreterRecMethod(Source.FRecSetList[i]) do
      AddRecSetEx(UnitName, JvInterpreterRecord.Identifier, Identifier, Func, ParamCount,
        ParamTypes, Data);
  for i := 0 to Source.FExtFunList.Count - 1 do
    with TJvInterpreterExtFun(Source.FExtFunList[i]) do
      AddExtFunEx(UnitName, Identifier, DllInstance, DllName, FunName, FunIndex,
        FunDesc.FParamCount, FunDesc.FParamTypes, FunDesc.FResTyp, Data);
  for i := 0 to Source.FEventHandlerList.Count - 1 do
    with TJvInterpreterEventDesc(Source.FEventHandlerList[i]) do
      AddHandlerEx(UnitName, Identifier, EventClass, Code, Data);
  for i := 0 to Source.FEventList.Count - 1 do
    with TJvInterpreterClass(Source.FEventList[i]) do
      AddEventEx(UnitName, FClassType, Identifier, Data);
  for i := 0 to Source.FOnGetList.Count - 1 do
    AddOnGet(TJvInterpreterGetValue(PMethod(Source.FOnGetList[i])^));
  for i := 0 to Source.FOnSetList.Count - 1 do
    AddOnSet(TJvInterpreterSetValue(PMethod(Source.FOnSetList[i])^));
end;

procedure TJvInterpreterAdapter.AddSrcUnit(Identifier: string; Source: string;
  UsesList: string);
begin
  AddSrcUnitEx(Identifier, Source, UsesList, nil);
end;

{ if unit with name 'Identifier' already exists its source will be replaced }

procedure TJvInterpreterAdapter.AddSrcUnitEx(Identifier: string; Source: string;
  UsesList: string; Data: Pointer);
var
  JvInterpreterUnit: TJvInterpreterSrcUnit;
  S: string;
  i: Integer;
  JvInterpreterIdentifier: TJvInterpreterIdentifier;
begin
  JvInterpreterUnit := nil;
  for i := 0 to FSrcUnitList.Count - 1 do
  begin
    JvInterpreterIdentifier := TJvInterpreterIdentifier(FSrcUnitList.Items[i]);
    if Cmp(JvInterpreterIdentifier.Identifier, Identifier) then
    begin
      JvInterpreterUnit := TJvInterpreterSrcUnit(FSrcUnitList.Items[i]);
      Break;
    end;
  end;
  if JvInterpreterUnit = nil then
  begin
    JvInterpreterUnit := TJvInterpreterSrcUnit.Create;
    FSrcUnitList.Add(JvInterpreterUnit);
  end;

  if JvInterpreterUnit.Source = '' then
  begin
    JvInterpreterUnit.Identifier := Identifier;
    JvInterpreterUnit.Source := Source;
    JvInterpreterUnit.Data := Data;
    i := 0;
    S := Trim(SubStr(UsesList, i, ','));
    while S <> '' do
    begin
      JvInterpreterUnit.UsesList[i] := S;
      Inc(i);
      S := Trim(SubStr(UsesList, i, ','));
    end;
  end;
end;

procedure TJvInterpreterAdapter.AddExtUnit(Identifier: string);
begin
  AddExtUnitEx(Identifier, nil);
end;

procedure TJvInterpreterAdapter.AddExtUnitEx(Identifier: string; Data: Pointer);
var
  JvInterpreterUnit: TJvInterpreterIdentifier;
begin
  JvInterpreterUnit := TJvInterpreterIdentifier.Create;
  JvInterpreterUnit.Identifier := Identifier;
  JvInterpreterUnit.Data := Data;
  FExtUnitList.Add(JvInterpreterUnit);
end;

procedure TJvInterpreterAdapter.AddClass(UnitName: string; AClassType: TClass;
  Identifier: string);
begin
  AddClassEx(UnitName, AClassType, Identifier, nil);
end;

procedure TJvInterpreterAdapter.AddClassEx(UnitName: string; AClassType: TClass;
  Identifier: string; Data: Pointer);
var
  JvInterpreterClass: TJvInterpreterClass;
begin
  JvInterpreterClass := TJvInterpreterClass.Create;
  JvInterpreterClass.FClassType := AClassType;
  JvInterpreterClass.Identifier := Identifier;
  JvInterpreterClass.Data := Data;
  FClassList.Add(JvInterpreterClass);
  FSorted := False; // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddGet(AClassType: TClass; Identifier: string;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word);
begin
  AddGetEx(AClassType, Identifier, GetFunc, ParamCount, ParamTypes, ResTyp, nil);
end;

procedure TJvInterpreterAdapter.AddGetEx(AClassType: TClass; Identifier: string;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word; Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterMethod;
begin
  JvInterpreterMethod := TJvInterpreterMethod.Create;
  JvInterpreterMethod.FClassType := AClassType;
  JvInterpreterMethod.Identifier := Identifier;
  JvInterpreterMethod.Func := @GetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.ResTyp := ResTyp;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FGetList.Add(JvInterpreterMethod);
  FSorted := False; // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddIGet(AClassType: TClass; Identifier: string;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word);
begin
  AddIGetEx(AClassType, Identifier, GetFunc, ParamCount, ParamTypes, ResTyp, nil);
end;

procedure TJvInterpreterAdapter.AddIGetEx(AClassType: TClass; Identifier: string;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word; Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterMethod;
begin
  JvInterpreterMethod := TJvInterpreterMethod.Create;
  JvInterpreterMethod.FClassType := AClassType;
  JvInterpreterMethod.Identifier := Identifier;
  JvInterpreterMethod.Func := @GetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.ResTyp := ResTyp;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FIGetList.Add(JvInterpreterMethod);
  FSorted := False; // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddIDGet(AClassType: TClass;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word);
begin
  AddIDGetEx(AClassType, GetFunc, ParamCount, ParamTypes, ResTyp, nil);
end;

procedure TJvInterpreterAdapter.AddIDGetEx(AClassType: TClass;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word; Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterMethod;
begin
  JvInterpreterMethod := TJvInterpreterMethod.Create;
  JvInterpreterMethod.FClassType := AClassType;
  JvInterpreterMethod.Func := @GetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.ResTyp := ResTyp;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FIDGetList.Add(JvInterpreterMethod);
end;

procedure TJvInterpreterAdapter.AddIntfGet(IID: TGUID; Identifier: string;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
  ParamTypes: array of Word; ResTyp: Word);
begin
  AddIntfGetEx(IID, Identifier, GetFunc, ParamCount, ParamTypes, ResTyp, nil);
end;

procedure TJvInterpreterAdapter.AddIntfGetEx(IID: TGUID; Identifier: string;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer;
  ParamTypes: array of Word; ResTyp: Word; Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterIntfMethod;
begin
  JvInterpreterMethod := TJvInterpreterIntfMethod.Create;
  JvInterpreterMethod.IID := IID;
  JvInterpreterMethod.Identifier := Identifier;
  JvInterpreterMethod.Func := @GetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.ResTyp := ResTyp;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FIntfGetList.Add(JvInterpreterMethod);
  FSorted := False;  // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddDGet(AClassType: TClass; Identifier: string;
  GetFunc: Pointer; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word; CallConvention: TCallConvention);
begin
  AddDGetEx(AClassType, Identifier, GetFunc, ParamCount, ParamTypes, ResTyp,
    CallConvention, nil);
end;

procedure TJvInterpreterAdapter.AddDGetEx(AClassType: TClass; Identifier: string;
  GetFunc: Pointer; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word; CallConvention: TCallConvention; Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterDMethod;
begin
  JvInterpreterMethod := TJvInterpreterDMethod.Create;
  JvInterpreterMethod.FClassType := AClassType;
  JvInterpreterMethod.Identifier := Identifier;
  JvInterpreterMethod.Func := GetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.ResTyp := ResTyp;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  JvInterpreterMethod.CallConvention := CallConvention;
  FDGetList.Add(JvInterpreterMethod);
end;

procedure TJvInterpreterAdapter.AddSet(AClassType: TClass; Identifier: string;
  SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer; ParamTypes: array of Word);
begin
  AddSetEx(AClassType, Identifier, SetFunc, ParamCount, ParamTypes, nil);
end;

procedure TJvInterpreterAdapter.AddSetEx(AClassType: TClass; Identifier: string;
  SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer; ParamTypes: array of Word;
  Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterMethod;
begin
  JvInterpreterMethod := TJvInterpreterMethod.Create;
  JvInterpreterMethod.FClassType := AClassType;
  JvInterpreterMethod.Identifier := Identifier;
  JvInterpreterMethod.Func := @SetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FSetList.Add(JvInterpreterMethod);
  FSorted := False; // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddISet(AClassType: TClass; Identifier: string;
  SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer; ParamTypes: array of Word);
begin
  AddISetEx(AClassType, Identifier, SetFunc, ParamCount, ParamTypes, nil);
end;

procedure TJvInterpreterAdapter.AddISetEx(AClassType: TClass; Identifier: string;
  SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer; ParamTypes: array of Word;
  Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterMethod;
begin
  JvInterpreterMethod := TJvInterpreterMethod.Create;
  JvInterpreterMethod.FClassType := AClassType;
  JvInterpreterMethod.Identifier := Identifier;
  JvInterpreterMethod.Func := @SetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FISetList.Add(JvInterpreterMethod);
  FSorted := False; // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddIDSet(AClassType: TClass;
  SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer; ParamTypes: array of Word);
begin
  AddIDSetEx(AClassType, SetFunc, ParamCount, ParamTypes, nil);
end;

procedure TJvInterpreterAdapter.AddIDSetEx(AClassType: TClass;
  SetFunc: TJvInterpreterAdapterSetValue; ParamCount: Integer; ParamTypes: array of Word;
  Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterMethod;
begin
  JvInterpreterMethod := TJvInterpreterMethod.Create;
  JvInterpreterMethod.FClassType := AClassType;
  JvInterpreterMethod.Func := @SetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FIDSetList.Add(JvInterpreterMethod);
end;

procedure TJvInterpreterAdapter.AddFun(UnitName: string; Identifier: string;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word);
begin
  AddFunEx(UnitName, Identifier, GetFunc, ParamCount, ParamTypes, ResTyp, nil);
end;

procedure TJvInterpreterAdapter.AddFunEx(UnitName: string; Identifier: string;
  GetFunc: TJvInterpreterAdapterGetValue; ParamCount: Integer; ParamTypes: array of Word;
  ResTyp: Word; Data: Pointer);
var
  JvInterpreterMethod: TJvInterpreterMethod;
begin
  JvInterpreterMethod := TJvInterpreterMethod.Create;
  JvInterpreterMethod.Identifier := Identifier;
  JvInterpreterMethod.Func := @GetFunc;
  JvInterpreterMethod.ParamCount := ParamCount;
  JvInterpreterMethod.ResTyp := ResTyp;
  JvInterpreterMethod.Data := Data;
  ConvertParamTypes(ParamTypes, JvInterpreterMethod.ParamTypes);
  FFunList.Add(JvInterpreterMethod);
  FSorted := False; // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddRec(UnitName: string; Identifier: string;
  RecordSize: Integer; Fields: array of TJvInterpreterRecField;
  CreateFunc: TJvInterpreterAdapterNewRecord; DestroyFunc: TJvInterpreterAdapterDisposeRecord;
  CopyFunc: TJvInterpreterAdapterCopyRecord);
begin
  AddRecEx(UnitName, Identifier, RecordSize, Fields, CreateFunc, DestroyFunc, CopyFunc, nil);
end;

procedure TJvInterpreterAdapter.AddRecEx(UnitName: string; Identifier: string;
  RecordSize: Integer; Fields: array of TJvInterpreterRecField;
  CreateFunc: TJvInterpreterAdapterNewRecord; DestroyFunc: TJvInterpreterAdapterDisposeRecord;
  CopyFunc: TJvInterpreterAdapterCopyRecord; Data: Pointer);
var
  JvInterpreterRecord: TJvInterpreterRecord;
  i: Integer;
begin
  JvInterpreterRecord := TJvInterpreterRecord.Create;
  JvInterpreterRecord.Identifier := Identifier;
  JvInterpreterRecord.RecordSize := RecordSize;
  JvInterpreterRecord.CreateFunc := CreateFunc;
  JvInterpreterRecord.DestroyFunc := DestroyFunc;
  JvInterpreterRecord.CopyFunc := CopyFunc;
  JvInterpreterRecord.Data := Data;
  for i := Low(Fields) to High(Fields) do
  begin
    JvInterpreterRecord.Fields[i] := Fields[i];
    JvInterpreterRecord.Fields[i].DataType := nil;
  end;
  JvInterpreterRecord.FieldCount := High(Fields) - Low(Fields) + 1;
  FRecList.Add(JvInterpreterRecord);
end;

procedure TJvInterpreterAdapter.AddRecGet(UnitName: string; RecordType: string;
  Identifier: string; GetFunc: TJvInterpreterAdapterGetValue;
  ParamCount: Integer; ParamTypes: array of Word; ResTyp: Word);
begin
  AddRecGetEx(UnitName, RecordType, Identifier, GetFunc, ParamCount,
    ParamTypes, ResTyp, nil);
end;

procedure TJvInterpreterAdapter.AddRecGetEx(UnitName: string; RecordType: string;
  Identifier: string; GetFunc: TJvInterpreterAdapterGetValue;
  ParamCount: Integer; ParamTypes: array of Word; ResTyp: Word; Data: Pointer);
var
  RecMethod: TJvInterpreterRecMethod;
begin
  RecMethod := TJvInterpreterRecMethod.Create;
  RecMethod.JvInterpreterRecord := GetRec(RecordType) as TJvInterpreterRecord;
  RecMethod.Identifier := Identifier;
  RecMethod.Func := @GetFunc;
  RecMethod.ParamCount := ParamCount;
  RecMethod.ResTyp := ResTyp;
  RecMethod.Data := Data;
  ConvertParamTypes(ParamTypes, RecMethod.ParamTypes);
  FRecGetList.Add(RecMethod);
end;

procedure TJvInterpreterAdapter.AddRecSet(UnitName: string; RecordType: string;
  Identifier: string; SetFunc: TJvInterpreterAdapterSetValue;
  ParamCount: Integer; ParamTypes: array of Word);
begin
  AddRecSetEx(UnitName, RecordType, Identifier, SetFunc, ParamCount, ParamTypes,
    nil);
end;

procedure TJvInterpreterAdapter.AddRecSetEx(UnitName: string; RecordType: string;
  Identifier: string; SetFunc: TJvInterpreterAdapterSetValue;
  ParamCount: Integer; ParamTypes: array of Word; Data: Pointer);
var
  RecMethod: TJvInterpreterRecMethod;
begin
  RecMethod := TJvInterpreterRecMethod.Create;
  RecMethod.JvInterpreterRecord := GetRec(RecordType) as TJvInterpreterRecord;
  RecMethod.Identifier := Identifier;
  RecMethod.Func := @SetFunc;
  RecMethod.ParamCount := ParamCount;
  RecMethod.Data := Data;
  ConvertParamTypes(ParamTypes, RecMethod.ParamTypes);
  FRecSetList.Add(RecMethod);
end;

procedure TJvInterpreterAdapter.AddConst(UnitName: string; Identifier: string;
  Value: Variant);
begin
  AddConstEx(UnitName, Identifier, Value, nil);
end;

procedure TJvInterpreterAdapter.AddConstEx(AUnitName: string; AIdentifier: string;
  AValue: Variant; AData: Pointer);
var
  JvInterpreterConst: TJvInterpreterConst;
begin
  JvInterpreterConst := TJvInterpreterConst.Create;
  JvInterpreterConst.Identifier := AIdentifier;
  JvInterpreterConst.Value := AValue;
  JvInterpreterConst.Data := AData;
  FConstList.Add(JvInterpreterConst);
  FSorted := False; // Ivan_ra
end;

procedure TJvInterpreterAdapter.AddExtFun(UnitName: string; Identifier: string;
  DllInstance: HINST; DllName: string; FunName: string; FunIndex: Integer;
  ParamCount: Integer; ParamTypes: array of Word; ResTyp: Word);
begin
  AddExtFunEx(UnitName, Identifier, DllInstance, DllName, FunName, FunIndex,
    ParamCount, ParamTypes, ResTyp, nil);
end;

procedure TJvInterpreterAdapter.AddExtFunEx(AUnitName: string; AIdentifier: string;
  ADllInstance: HINST; ADllName: string; AFunName: string; AFunIndex: Integer;
  AParamCount: Integer; AParamTypes: array of Word; AResTyp: Word; AData: Pointer);
var
  JvInterpreterExtFun: TJvInterpreterExtFun;
begin
  JvInterpreterExtFun := TJvInterpreterExtFun.Create;
  with JvInterpreterExtFun do
  begin
    FunDesc.FUnitName := AUnitName;
    Identifier := AIdentifier;
    DllInstance := ADllInstance;
    DllName := ADllName;
    FunName := AFunName;
    FunIndex := AFunIndex;
    FunDesc.FParamCount := AParamCount;
    FunDesc.FResTyp := AResTyp;
    Data := AData;
    ConvertParamTypes(AParamTypes, FunDesc.FParamTypes);
  end;
  FExtFunList.Add(JvInterpreterExtFun);
end;

procedure TJvInterpreterAdapter.AddSrcFun(UnitName: string; Identifier: string;
  PosBeg, PosEnd: Integer; ParamCount: Integer; ParamTypes: array of Word;
  ParamNames: array of string; ResTyp: Word;
  AResDataType: IJvInterpreterDataType;
  Data: Pointer);
begin
  AddSrcFunEx(UnitName, Identifier, PosBeg, PosEnd, ParamCount, ParamTypes,
    ParamNames, ResTyp, AResDataType, nil);
end;

procedure TJvInterpreterAdapter.AddSrcFunEx(AUnitName: string; AIdentifier: string;
  APosBeg, APosEnd: Integer; AParamCount: Integer; AParamTypes: array of Word;
  AParamNames: array of string; AResTyp: Word;
  AResDataType: IJvInterpreterDataType;
  AData: Pointer);
var
  JvInterpreterSrcFun: TJvInterpreterSrcFun;
begin
  JvInterpreterSrcFun := TJvInterpreterSrcFun.Create;
  with JvInterpreterSrcFun do
  begin
    FunDesc.FUnitName := AUnitName;
    FunDesc.FIdentifier := AIdentifier;
    FunDesc.FPosBeg := APosBeg;
    FunDesc.FPosEnd := APosEnd;
    FunDesc.FParamCount := AParamCount;
    FunDesc.FResTyp := AResTyp;
    FunDesc.FResDataType := AResDataType;
    Identifier := AIdentifier;
    Data := AData;
    ConvertParamTypes(AParamTypes, FunDesc.FParamTypes);
    ConvertParamNames(AParamNames, FunDesc.FParamNames);
    FunDesc.FResTyp := AResTyp;
  end;
  FSrcFunList.Add(JvInterpreterSrcFun);
end;

procedure TJvInterpreterAdapter.AddHandler(UnitName: string; Identifier: string;
  EventClass: TJvInterpreterEventClass; Code: Pointer);
begin
  AddHandlerEx(UnitName, Identifier, EventClass, Code, nil);
end;

procedure TJvInterpreterAdapter.AddHandlerEx(AUnitName: string; AIdentifier: string;
  AEventClass: TJvInterpreterEventClass; ACode: Pointer; AData: Pointer);
var
  JvInterpreterEventDesc: TJvInterpreterEventDesc;
begin
  JvInterpreterEventDesc := TJvInterpreterEventDesc.Create;
  with JvInterpreterEventDesc do
  begin
    UnitName := AUnitName;
    Identifier := AIdentifier;
    EventClass := AEventClass;
    Code := ACode;
    Data := AData;
  end;
  FEventHandlerList.Add(JvInterpreterEventDesc);
end;

procedure TJvInterpreterAdapter.AddEvent(UnitName: string; AClassType: TClass;
  Identifier: string);
begin
  AddEventEx(UnitName, AClassType, Identifier, nil);
end;

procedure TJvInterpreterAdapter.AddEventEx(AUnitName: string; AClassType: TClass;
  AIdentifier: string; AData: Pointer);
var
  JvInterpreterEvent: TJvInterpreterClass;
begin
  JvInterpreterEvent := TJvInterpreterClass.Create;
  with JvInterpreterEvent do
  begin
    UnitName := AUnitName;
    Identifier := AIdentifier;
    FClassType := AClassType;
    Data := AData;
  end;
  FEventList.Add(JvInterpreterEvent);
end;

procedure TJvInterpreterAdapter.AddSrcVar(UnitName: string; Identifier, Typ: string;
  VTyp: Word; const Value: Variant; DataType: IJvInterpreterDataType);
begin
  FSrcVarList.AddVar(UnitName, Identifier, Typ, VTyp, Value, DataType);
end;

procedure TJvInterpreterAdapter.AddSrcClass(JvInterpreterSrcClass: TJvInterpreterIdentifier);
begin
  FSrcClassList.Add(JvInterpreterSrcClass);
end;

function TJvInterpreterAdapter.GetSrcClass(Identifier: string): TJvInterpreterIdentifier;
begin
  Result := FSrcClassList.IndexOf('', Identifier);
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
  for i := 0 to FRecList.Count - 1 do
  begin
    Result := FRecList[i];
    if Cmp(TJvInterpreterRecord(Result).Identifier, RecordType) then
      Exit;
  end;

  Result := nil;
end;

procedure TJvInterpreterAdapter.CheckArgs(var Args: TJvInterpreterArgs; ParamCount: Integer;
  var ParamTypes: TTypeArray);
var
  i: Integer;
begin
  if ParamCount = prArgsNoCheck then
    Exit;
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
end;

procedure TJvInterpreterAdapter.CheckAction(Expression: TJvInterpreterExpression;
  Args: TJvInterpreterArgs; Data: Pointer);
begin
  // abstract
end;

function TJvInterpreterAdapter.FindFunDesc(const UnitName: string;
  const Identifier: string): TJvInterpreterFunDesc;
var
  i: Integer;
begin
  for i := FSrcFunList.Count - 1 downto 0 do
  begin
    Result := TJvInterpreterSrcFun(FSrcFunList.Items[i]).FunDesc;
    if Cmp(Result.Identifier, Identifier) and
      (Cmp(Result.UnitName, UnitName) or (UnitName = '')) then
      Exit;
  end;
  if UnitName <> '' then
    Result := FindFunDesc('', Identifier)
  else
    Result := nil;
end;

function TJvInterpreterAdapter.GetValue(Expression: TJvInterpreterExpression; Identifier: string;
  var Value: Variant; Args: TJvInterpreterArgs): Boolean;

  function GetMethod: Boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterMethod;
  begin
    Result := GetValueRTTI(Identifier, Value, Args);
    if Result then
      Exit;
    if FGetList.Find(Identifier, i) then
      for i := i to FGetList.Count - 1 do
      begin
        JvInterpreterMethod := TJvInterpreterMethod(FGetList[i]);
        if Assigned(JvInterpreterMethod.Func) and
          (((Args.ObjTyp = varObject) and
          (Args.Obj is JvInterpreterMethod.FClassType)) or
          ((Args.ObjTyp = varClass) and
          (TClass(Args.Obj) = JvInterpreterMethod.FClassType))) {?!} then
        begin
          Args.Identifier := Identifier;
          CheckAction(Expression, Args, JvInterpreterMethod.Data);
          CheckArgs(Args, JvInterpreterMethod.ParamCount, JvInterpreterMethod.ParamTypes);
          TJvInterpreterAdapterGetValue(JvInterpreterMethod.Func)(Value, Args);
          Result := True;
          Exit;
        end;
        if not Cmp(JvInterpreterMethod.Identifier, Identifier) then
          Break;
      end;
    if Cmp(Identifier, 'Free') then
    begin
      Result := True;
      Args.Obj.Free;
      Args.Obj := nil;
      Value := Null;
      Exit;
    end;
  end;

  function IntfGetMethod: boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterIntfMethod;
    Intf: IUnknown;
  begin
    Result := False;
    if FIntfGetList.Find(Identifier, i) then
      for i := i to FIntfGetList.Count - 1 do
      begin
        JvInterpreterMethod := TJvInterpreterIntfMethod(FIntfGetList[i]);
        if Assigned(JvInterpreterMethod.Func) and
          ((Args.ObjTyp = varUnknown) and
          (IUnknown(Pointer(Args.Obj)).QueryInterface(JvInterpreterMethod.IID, Intf) = S_OK)) then
        begin
          Args.Identifier := Identifier;
          CheckAction(Expression, Args, JvInterpreterMethod.Data);
          CheckArgs(Args, JvInterpreterMethod.ParamCount,
            JvInterpreterMethod.ParamTypes);
          TJvInterpreterAdapterGetValue(JvInterpreterMethod.Func)(Value, Args);
          Result := True;
          Exit;
        end;
        if not Cmp(JvInterpreterMethod.Identifier, Identifier) then
          Break;
      end;
  end;

  function IGetMethod: Boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterMethod;
  begin
    if FIGetList.Find(Identifier, i) then
      for i := i to FIGetList.Count - 1 do
      begin
        JvInterpreterMethod := TJvInterpreterMethod(FIGetList[i]);
        if Assigned(JvInterpreterMethod.Func) and
          (((Args.ObjTyp = varObject) and
          (Args.Obj is JvInterpreterMethod.FClassType)) or
          ((Args.ObjTyp = varClass) and
          (TClass(Args.Obj) = JvInterpreterMethod.FClassType))) {?!} then
        begin
          Args.Identifier := Identifier;
          CheckAction(Expression, Args, JvInterpreterMethod.Data);
          CheckArgs(Args, JvInterpreterMethod.ParamCount, JvInterpreterMethod.ParamTypes);
          TJvInterpreterAdapterGetValue(JvInterpreterMethod.Func)(Value, Args);
          Result := True;
          Args.ReturnIndexed := True;
          Exit;
        end;
        if not Cmp(JvInterpreterMethod.Identifier, Identifier) then
          Break;
      end;
    Result := False;
  end;

  { function DGetMethod is under construction }

  function DGetMethod: Boolean;
  var
    JvInterpreterMethod: TJvInterpreterDMethod;
    i, j: Integer;
    Aint: Integer;
    Aword: Word;
    iRes: Integer;
    Func: Pointer;
    REAX, REDX, RECX: Integer;
  begin
    Result := False;
    iRes := 0; { satisfy compiler }
    for i := 0 to FDGetList.Count - 1 do
    begin
      JvInterpreterMethod := TJvInterpreterDMethod(FDGetList[i]);
      Func := JvInterpreterMethod.Func;
      if Assigned(JvInterpreterMethod.Func) and
        (((Args.ObjTyp = varObject) and
        (Args.Obj is JvInterpreterMethod.FClassType)) { or
          ((Args.ObjTyp = varClass) and
          (TClass(Args.Obj) = JvInterpreterMethod.FClassType))}) and
        Cmp(JvInterpreterMethod.Identifier, Identifier) then
      begin
        Args.Identifier := Identifier;
        CheckAction(Expression, Args, JvInterpreterMethod.Data);
        CheckArgs(Args, JvInterpreterMethod.ParamCount, JvInterpreterMethod.ParamTypes);
        if ccFastCall in JvInterpreterMethod.CallConvention then
        begin
          { !!! Delphi fast-call !!! }
          { push parameters to stack }
          for j := 2 to JvInterpreterMethod.ParamCount - 1 do
            if (JvInterpreterMethod.ParamTypes[j] = varInteger)or
            (JvInterpreterMethod.ParamTypes[j] = varObject) or
            (JvInterpreterMethod.ParamTypes[j] = varPointer) or
            (JvInterpreterMethod.ParamTypes[j] = varBoolean){?} then
            begin
              Aint := Args.Values[j];
              asm push Aint
              end;
            end
            else if JvInterpreterMethod.ParamTypes[j] = varSmallInt then
            begin
              Aword := Word(Args.Values[j]);
              asm push Aword
              end;
            end
            else
              JvInterpreterErrorN(ieDirectInvalidArgument, -1, Identifier);

          REAX := Integer(Args.Obj);
          if JvInterpreterMethod.ParamCount > 0 then
            if (JvInterpreterMethod.ParamTypes[0] = varInteger) or
            (JvInterpreterMethod.ParamTypes[0] = varObject) or
            (JvInterpreterMethod.ParamTypes[0] = varPointer) or
            (JvInterpreterMethod.ParamTypes[0] =  varBoolean) or
            (JvInterpreterMethod.ParamTypes[0] = varSmallInt) or
            (JvInterpreterMethod.ParamTypes[0] = varString) then
                REDX := TVarData(Args.Values[0]).vInteger
            else
              JvInterpreterErrorN(ieDirectInvalidArgument, -1, Identifier);

          if JvInterpreterMethod.ParamCount > 1 then
            if (JvInterpreterMethod.ParamTypes[1] = varInteger) or
            (JvInterpreterMethod.ParamTypes[1] = varObject) or
            (JvInterpreterMethod.ParamTypes[1] = varPointer) or
            (JvInterpreterMethod.ParamTypes[1] = varBoolean) or
            (JvInterpreterMethod.ParamTypes[1] = varSmallInt) or
            (JvInterpreterMethod.ParamTypes[1] = varString) then
                RECX := TVarData(Args.Values[1]).vInteger
            else
              JvInterpreterErrorN(ieDirectInvalidArgument, -1, Identifier);

          if (JvInterpreterMethod.ResTyp = varSmallInt) or
          (JvInterpreterMethod.ResTyp = varInteger) or
          (JvInterpreterMethod.ResTyp = varBoolean) or
          (JvInterpreterMethod.ResTyp = varEmpty) or
          (JvInterpreterMethod.ResTyp = varObject) or
          (JvInterpreterMethod.ResTyp = varPointer) then
            asm
              mov      EAX, REAX
              mov      EDX, REDX
              mov      ECX, RECX
              call     Func
              mov      iRes, EAX
            end
          else
            JvInterpreterErrorN(ieDirectInvalidResult, -1, Identifier);

          { clear result }
          if (JvInterpreterMethod.ResTyp = varInteger) or
          (JvInterpreterMethod.ResTyp = varObject) then
            Value := iRes
          else if (JvInterpreterMethod.ResTyp = varSmallInt) then
            Value := iRes and $0000FFFF
          else if (JvInterpreterMethod.ResTyp = varBoolean) then
            begin
              Value := iRes and $000000FF;
              TVarData(Value).VType := varBoolean;
            end
          else if (JvInterpreterMethod.ResTyp = varEmpty) then
              Value := Null;
        end
        else
          JvInterpreterErrorN(ieDirectInvalidConvention, -1, Identifier);
        Result := True;
        Exit;
      end;
    end;
  end;

  function GetRecord: Boolean;
  var
    i: Integer;
    JvInterpreterRecord: TJvInterpreterRecord;
    Rec: PChar;
    JvInterpreterRecMethod: TJvInterpreterRecMethod;
  begin
    Result := False;
    JvInterpreterRecord := (Args.Obj as TJvInterpreterRecHolder).JvInterpreterRecord;
    for i := 0 to JvInterpreterRecord.FieldCount - 1 do
      if Cmp(JvInterpreterRecord.Fields[i].Identifier, Identifier) then
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
            varEmpty:
              JvInterpreterVarCopy(Value, Variant(PVarData(Rec + Offset)^));
          end;
        Result := True;
        Exit;
      end;
    for i := 0 to FRecGetList.Count - 1 do
    begin
      JvInterpreterRecMethod := TJvInterpreterRecMethod(FRecGetList[i]);
      if (JvInterpreterRecMethod.JvInterpreterRecord = JvInterpreterRecord) and
        Cmp(JvInterpreterRecMethod.Identifier, Identifier) then
      begin
        Args.Identifier := Identifier;
        CheckArgs(Args, JvInterpreterRecMethod.ParamCount, JvInterpreterRecMethod.ParamTypes);
        TJvInterpreterAdapterGetValue(JvInterpreterRecMethod.Func)(Value, Args);
        Result := True;
        Exit;
      end;
    end
  end;

  function GetConst: Boolean;
  var
    i: Integer;
    JvInterpreterConst: TJvInterpreterConst;
  begin
    if Cmp(Identifier, 'nil') then
    begin
      Value := P2V(nil);
      Result := True;
      Exit;
    end;
    if Cmp(Identifier, 'Null') then
    begin
      Value := Null;
      Result := True;
      Exit;
    end;
    Result := FConstList.Find(Identifier, i);
    if Result then
    begin
      JvInterpreterConst := TJvInterpreterConst(FConstList[i]);
      CheckAction(Expression, Args, JvInterpreterConst.Data);
      Value := JvInterpreterConst.Value;
    end;
  end;

  function GetClass: Boolean;
  var
    i: Integer;
    JvInterpreterClass: TJvInterpreterClass;
  begin
    Result := FClassList.Find(Identifier, i);
    if Result then
    begin
      JvInterpreterClass := TJvInterpreterClass(FClassList[i]);
      if Args.Count = 0 then
        Value := C2V(JvInterpreterClass.FClassType)
      else if Args.Count = 1 then
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
  end;

  function GetFun: Boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterMethod;
  begin
    Result := FFunList.Find(Identifier, i);
    if Result then
    begin
      JvInterpreterMethod := TJvInterpreterMethod(FFunList[i]);
      if Cmp(JvInterpreterMethod.Identifier, Identifier) then
      begin
        Args.Identifier := Identifier;
        CheckAction(Expression, Args, JvInterpreterMethod.Data);
        CheckArgs(Args, JvInterpreterMethod.ParamCount, JvInterpreterMethod.ParamTypes);
        TJvInterpreterAdapterGetValue(JvInterpreterMethod.Func)(Value, Args);
      end;
    end;
  end;

  function GetExtFun: Boolean;
  var
    i: Integer;
    JvInterpreterExtFun: TJvInterpreterExtFun;
  begin
    for i := 0 to FExtFunList.Count - 1 do { Iterate }
    begin
      JvInterpreterExtFun := TJvInterpreterExtFun(FExtFunList[i]);
      if Cmp(JvInterpreterExtFun.Identifier, Identifier) then
      begin
        Args.Identifier := Identifier;
        CheckAction(Expression, Args, JvInterpreterExtFun.Data);
        CheckArgs(Args, JvInterpreterExtFun.FunDesc.ParamCount,
          JvInterpreterExtFun.FunDesc.FParamTypes);
        Value := JvInterpreterExtFun.CallDll(Args);
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;

  function GetSrcVar: Boolean;
  begin
    Result := FSrcVarList.GetValue(Identifier, Value, Args);
  end;

  function GetSrcUnit: Boolean;
  var
    i: Integer;
    JvInterpreterSrcUnit: TJvInterpreterSrcUnit;
    FParams: TTypeArray;
  begin
    for i := 0 to FSrcUnitList.Count - 1 do { Iterate }
    begin
      JvInterpreterSrcUnit := TJvInterpreterSrcUnit(FSrcUnitList[i]);
      if Cmp(JvInterpreterSrcUnit.Identifier, Identifier) then
      begin
        CheckArgs(Args, 0, FParams);
        Value := O2V(JvInterpreterSrcUnit);
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;

{$IFDEF JvInterpreter_OLEAUTO}

  function GetOleAutoFun: Boolean;
  var
    FParams: TTypeArray;
  begin
    Result := False;
    if Cmp(Identifier, 'CreateOleObject') or
      Cmp(Identifier, 'GetActiveOleObject') or
      Cmp(Identifier, 'GetOleObject') then
    begin
      FParams[0] := varString;
      CheckArgs(Args, 1, FParams);
      if Cmp(Identifier, 'CreateOleObject') then
        Value := CreateOleObject(Args.Values[0])
      else if Cmp(Identifier, 'CreateOleObject') then
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
  end;
{$ENDIF JvInterpreter_OLEAUTO}

  function TypeCast: Boolean;
  var
    VT: Word;
  begin
    VT := TypeName2VarTyp(Identifier);
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
  if not FSorted then
    Sort;

  if Args.Indexed then
  begin
    if Args.ObjTyp = varRecord then
    begin
      if (Args.Obj is TJvInterpreterRecHolder) and GetRecord then
      begin
        Args.ReturnIndexed := False;
        Exit;
      end;
    end
    else if (Args.Obj <> nil) and ((Args.ObjTyp = varObject) or (Args.ObjTyp = varClass)) then
      if IGetMethod then
        Exit;
  end
  else
  begin
    if Args.Obj <> nil then
    begin
      { methods }
      if (Args.ObjTyp = varObject) or (Args.ObjTyp = varClass) then
      begin
        if GetMethod or DGetMethod then
          Exit;
      end
      else if Args.ObjTyp = varUnknown then
      begin
        if IntfGetMethod then
          Exit;
      end
      else if Args.ObjTyp = varRecord then
      begin
        if (Args.Obj is TJvInterpreterRecHolder) and GetRecord then
          Exit;
      end
      else if Args.ObjTyp = varDispatch then
      { Ole automation call }
      begin
{$IFDEF JvInterpreter_OLEAUTO}
        Result := DispatchCall(Identifier, Value, Args, True);
        if Result then
          Exit;
{$ELSE}
        NotImplemented('Ole automation call');
{$ENDIF JvInterpreter_OLEAUTO}
      end;
    end
    else
    begin
      { classes }
      if GetClass then
        Exit;
      { constants }
      if GetConst then
        Exit;
      { classless functions and procedures }
      if GetFun then
        Exit;
      { external functions }
      if GetExtFun then
        Exit;
{$IFDEF JvInterpreter_OLEAUTO}
      if GetOleAutoFun then
        Exit;
{$ENDIF JvInterpreter_OLEAUTO}
      if TypeCast then
        Exit;
    end;
  end;

  { source variables and constants }
  if GetSrcVar then
    Exit;

  if not ((Args.Obj <> nil) and ((Args.ObjTyp = varObject) or (Args.ObjTyp = varClass))) then // ivan_ra
    if GetSrcUnit then
      Exit;

  for i := 0 to FOnGetList.Count - 1 do { Iterate }
  begin
    TJvInterpreterGetValue(FOnGetList[i]^)(Self, Identifier, Value, Args, Result);
    if Result then
      Exit;
  end;
  Result := False;
end;

function TJvInterpreterAdapter.SetValue(Expression: TJvInterpreterExpression; Identifier: string;
  const Value: Variant; Args: TJvInterpreterArgs): Boolean;
var
  i: Integer;
{$IFDEF JvInterpreter_OLEAUTO}
  V: Variant;
{$ENDIF JvInterpreter_OLEAUTO}

  function SetMethod: Boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterMethod;
  begin
    Result := SetValueRTTI(Identifier, Value, Args);
    if Result then
      Exit;
    for i := 0 to FSetList.Count - 1 do
    begin
      JvInterpreterMethod := TJvInterpreterMethod(FSetList[i]);
      if Assigned(JvInterpreterMethod.Func) and
        (Args.Obj is JvInterpreterMethod.FClassType) and
        Cmp(JvInterpreterMethod.Identifier, Identifier) then
      begin
        Args.Identifier := Identifier;
        CheckAction(Expression, Args, JvInterpreterMethod.Data);
        CheckArgs(Args, JvInterpreterMethod.ParamCount, JvInterpreterMethod.ParamTypes);
        TJvInterpreterAdapterSetValue(JvInterpreterMethod.Func)(Value, Args);
        Result := True;
        Exit;
      end;
    end;
  end;

  function ISetMethod: Boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterMethod;
  begin
    Result := False;
    if FISetList.Find(Identifier, i) then
      for i := i to FISetList.Count - 1 do
      begin
        JvInterpreterMethod := TJvInterpreterMethod(FISetList[i]);
        if Assigned(JvInterpreterMethod.Func) and
          (Args.Obj is JvInterpreterMethod.FClassType) and
          Cmp(JvInterpreterMethod.Identifier, Identifier) then
        begin
          Args.Identifier := Identifier;
          CheckAction(Expression, Args, JvInterpreterMethod.Data);
          CheckArgs(Args, JvInterpreterMethod.ParamCount, JvInterpreterMethod.ParamTypes);
          TJvInterpreterAdapterSetValue(JvInterpreterMethod.Func)(Value, Args);
          Result := True;
          Args.ReturnIndexed := True;
          Exit;
        end;
        if not Cmp(JvInterpreterMethod.Identifier, Identifier) then
          Break;
      end;
  end;

  function SetRecord: Boolean;
  var
    i: Integer;
    JvInterpreterRecord: TJvInterpreterRecord;
    JvInterpreterRecMethod: TJvInterpreterRecMethod;
    Rec: PChar;
  begin
    Result := False;
    JvInterpreterRecord := (Args.Obj as TJvInterpreterRecHolder).JvInterpreterRecord;
    for i := 0 to JvInterpreterRecord.FieldCount - 1 do
      if Cmp(JvInterpreterRecord.Fields[i].Identifier, Identifier) then
      begin
        Rec := P2R(Args.Obj);
        with JvInterpreterRecord.Fields[i] do
          case Typ of
            varInteger:
              PInteger(Rec + Offset)^ := Value;
            varSmallInt:
              PWord(Rec + Offset)^ := Word(Value);
            varBoolean:
              PBool(Rec + Offset)^ := Value;
            varEmpty:
              JvInterpreterVarAssignment(Variant(PVarData(Rec + Offset)^), Value);
          end;
        Result := True;
        Exit;
      end;
    for i := 0 to FRecSetList.Count - 1 do
    begin
      JvInterpreterRecMethod := TJvInterpreterRecMethod(FRecSetList[i]);
      if (JvInterpreterRecMethod.JvInterpreterRecord = JvInterpreterRecord) and
        Cmp(JvInterpreterRecMethod.Identifier, Identifier) then
      begin
        Args.Identifier := Identifier;
        CheckArgs(Args, JvInterpreterRecMethod.ParamCount, JvInterpreterRecMethod.ParamTypes);
        TJvInterpreterAdapterSetValue(JvInterpreterRecMethod.Func)(Value, Args);
        Result := True;
        Exit;
      end;
    end;
  end;

  function SetSrcVar: Boolean;
  begin
    Result := FSrcVarList.SetValue(Identifier, Value, Args);
  end;

begin
  Result := True;
  if not FSorted then
    Sort;

  if Args.Indexed then
  begin
    if (Args.Obj <> nil) and ((Args.ObjTyp = varObject) or (Args.ObjTyp = varClass)) then
      if ISetMethod then
        Exit;
  end
  else
  begin
    if Args.Obj <> nil then
    begin
      { methods }
      if (Args.ObjTyp = varObject) or (Args.ObjTyp = varClass) then
      begin
        if SetMethod then
          Exit;
      end
      else if Args.ObjTyp = varRecord then
      begin
        if (Args.Obj is TJvInterpreterRecHolder) and SetRecord then
          Exit;
      end
      else if Args.ObjTyp = varDispatch then
      { Ole automation call }
      begin
{$IFDEF JvInterpreter_OLEAUTO}
        V := Value;
        Result := DispatchCall(Identifier, V, Args, False);
        if Result then
          Exit;
{$ELSE}
        NotImplemented('Ole automation call');
{$ENDIF JvInterpreter_OLEAUTO}
      end;
    end;
  end;

  { source variables and constants }
  if SetSrcVar then
    Exit;

  for i := 0 to FOnSetList.Count - 1 do { Iterate }
  begin
    TJvInterpreterSetValue(FOnSetList[i]^)(Self, Identifier, Value, Args, Result);
    if Result then
      Exit;
  end;
  Result := False;
end;

function TJvInterpreterAdapter.GetElement(Expression: TJvInterpreterExpression;
  const Variable: Variant; var Value: Variant; var Args: TJvInterpreterArgs): Boolean;

  function GetID: Boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterMethod;
    Obj: TObject;
  begin
    Obj := V2O(Variable);
    for i := 0 to FIDGetList.Count - 1 do { Iterate }
    begin
      JvInterpreterMethod := TJvInterpreterMethod(FIDGetList[i]);
      if Obj is JvInterpreterMethod.FClassType then
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
  { default indexed properties }
  if TVarData(Variable).VType = varObject then
  begin
    if GetID then
      Exit;
    Result := False;
  end
  else
    Result := False;
end;

function TJvInterpreterAdapter.SetElement(Expression: TJvInterpreterExpression;
  var Variable: Variant; const Value: Variant; var Args: TJvInterpreterArgs): Boolean;

  function SetID: Boolean;
  var
    i: Integer;
    JvInterpreterMethod: TJvInterpreterMethod;
    Obj: TObject;
  begin
    Obj := V2O(Variable);
    for i := 0 to FIDSetList.Count - 1 do { Iterate }
    begin
      JvInterpreterMethod := TJvInterpreterMethod(FIDSetList[i]);
      if Obj is JvInterpreterMethod.FClassType then
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
  { default indexed properties }
  if TVarData(Variable).VType = varObject then
  begin
    if SetID then
      Exit;
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
  JvInterpreterRecord: TJvInterpreterRecord;
begin
  JvInterpreterRecord := TJvInterpreterRecord(GetRec(RecordType));
  if JvInterpreterRecord = nil then
    Result := False
  else
  begin
    JvInterpreterRecord.NewRecord(Value);
    Result := True;
  end;
end;

{$IFDEF JvInterpreter_OLEAUTO}

function TJvInterpreterAdapter.DispatchCall(Identifier: string; var Value: Variant;
  Args: TJvInterpreterArgs; Get: Boolean): Boolean; stdcall;
var
  CallDesc: TCallDesc;
  ParamTypes: array[0..MaxDispArgs * 4 - 1] of Byte;
  Ptr: Integer;
  TypePtr: Integer;
  PVRes: PVariant;
  Names: string;
  i: Integer;

  procedure AddParam(const Param: Variant);
  var
    Int: Integer;
    Wrd: WordBool;
    Poin: Pointer;
    Dbl: Double;
    //TempDisp : IDispatch; ComObj

    procedure AddParam1(Typ: Byte; ParamSize: Integer; const Param);
    begin
     { CallDesc.ArgTypes[Ptr] := Typ;
      Move(Param, ParamTypes[Ptr], ParamSize);
      Inc(Ptr, ParamSize); }
      CallDesc.ArgTypes[TypePtr] := Typ;
      Move(Param, ParamTypes[Ptr], ParamSize);
      Inc(Ptr, ParamSize);
      Inc(TypePtr);
    end;

  begin
    case TVarData(Param).VType of
      varInteger:
        begin
          Int := Param;
          AddParam1(varInteger, SizeOf(Int), Int);
        end;
      varDouble, varCurrency:
        begin
          Dbl := Param;
          AddParam1(varDouble, SizeOf(Dbl), Dbl);
        end;
      varString:
        begin
          Poin := V2P(Param);
          AddParam1(varStrArg, SizeOf(Poin), Poin);
        end;
      varBoolean:
        begin
          Wrd := WordBool(Param);
          AddParam1(varBoolean, SizeOf(Wrd), Wrd);
        end;
     { varDispatch:
        begin
          TempDisp := VarToInterface(Param.IFace);
          AddParam1(varDispatch, SizeOf(TempDisp), TempDisp);
        end; }

    end;
  end;

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
  Names := Identifier + #00;
  Ptr := 0;
  TypePtr := 0;
  if not Get then
  begin
    AddParam(Value);
    Inc(CallDesc.ArgCount);
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
    // (rom) absolute removed
    VarDispInvoke(PVRes, Args.Obj, PChar(Names), @CallDesc, @ParamTypes[0]);
  except
    on E: EOleError do
      JvInterpreterErrorN2(ieOleAuto, -1, Identifier, E.Message);
  end;
  if Get and (TVarData(Value).VType = varOleStr) then
    Value := VarAsType(Value, varString);
end;
{$ENDIF JvInterpreter_OLEAUTO}

function TJvInterpreterAdapter.GetValueRTTI(Identifier: string; var Value: Variant;
  Args: TJvInterpreterArgs): Boolean;
var
  TypeInf: PTypeInfo;
  PropInf: PPropInfo;
  PropTyp: TypInfo.TTypeKind;
begin
  Result := False;
  if (Args.ObjTyp <> varObject) or (Args.Obj = nil) then
    Exit;
  TypeInf := Args.Obj.ClassInfo;
  if TypeInf = nil then
    Exit;
  PropInf := GetPropInfo(TypeInf, Identifier);
  if PropInf = nil then
    Exit;
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
    {$IFDEF COMPILER6_UP}
    tkInterface:
      Value := GetInterfaceProp(Args.Obj, PropInf)
    {$ENDIF COMPILER6_UP}
  else
    Exit;
  end;
  if PropInf^.PropType^.Name = 'Boolean' then
    TVarData(Value).VType := varBoolean;
  Result := True;
end;

function TJvInterpreterAdapter.SetValueRTTI(Identifier: string; const Value: Variant;
  Args: TJvInterpreterArgs): Boolean;
var
  TypeInf: PTypeInfo;
  PropInf: PPropInfo;
  PropTyp: TypInfo.TTypeKind;
  Obj: TObject;
begin
  Result := False;
  if (Args.ObjTyp <> varObject) or (Args.Obj = nil) then
    Exit;
  Obj := Args.Obj;
  TypeInf := Obj.ClassInfo;
  if TypeInf = nil then
    Exit;
  PropInf := GetPropInfo(TypeInf, Identifier);
  if PropInf = nil then
    Exit;
  PropTyp := PropInf.PropType^.Kind;
  case PropTyp of
    tkInteger, tkEnumeration:
      SetOrdProp(Args.Obj, PropInf, Var2Type(Value, varInteger));
    tkChar, tkWChar:
      SetOrdProp(Args.Obj, PropInf, Integer(string(Value)[1]));
    tkFloat:
      SetFloatProp(Args.Obj, PropInf, Value);
    tkString, tkLString{$IFDEF COMPILER3_UP}, tkWString{$ENDIF COMPILER3_UP}:
      SetStrProp(Args.Obj, PropInf, VarToStr(Value));
    tkClass:
      SetOrdProp(Args.Obj, PropInf, Integer(V2O(Value)));
    tkSet:
      SetOrdProp(Args.Obj, PropInf, V2S(Value));
    {$IFDEF COMPILER6_UP}
    tkInterface:
      SetInterfaceProp(Args.Obj, PropInf, Value)
    {$ENDIF COMPILER6_UP}
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
  for i := 0 to FSrcUnitList.Count - 1 do
  begin
    JvInterpreterUnitSource := TJvInterpreterSrcUnit(FSrcUnitList.Items[i]);
    if Cmp(TJvInterpreterSrcUnit(JvInterpreterUnitSource).Identifier, NewUnitName) then
    begin
      Source := TJvInterpreterSrcUnit(JvInterpreterUnitSource).Source;
      Exit;
    end;
  end;
  Source := '';
end;

function TJvInterpreterAdapter.UnitExists(const Identifier: string): Boolean;
var
  JvInterpreterIdentifier: TJvInterpreterIdentifier;
  i: Integer;
begin
  Result := True;
  for i := 0 to FSrcUnitList.Count - 1 do
  begin
    JvInterpreterIdentifier := TJvInterpreterIdentifier(FSrcUnitList.Items[i]);
    if Cmp(JvInterpreterIdentifier.Identifier, Identifier) then
      Exit;
  end;
  for i := 0 to FExtUnitList.Count - 1 do
  begin
    JvInterpreterIdentifier := TJvInterpreterIdentifier(FExtUnitList.Items[i]);
    if Cmp(JvInterpreterIdentifier.Identifier, Identifier) then
      Exit;
  end;
  Result := False;
end;

function TJvInterpreterAdapter.NewEvent(const UnitName: string; const FunName,
  EventType: string; AOwner: TJvInterpreterExpression; AObject: TObject): TSimpleEvent;
var
  Event: TJvInterpreterEvent;
  i: Integer;
  JvInterpreterEventDesc: TJvInterpreterEventDesc;
begin
  for i := 0 to FEventHandlerList.Count - 1 do
  begin
    JvInterpreterEventDesc := TJvInterpreterEventDesc(FEventHandlerList.Items[i]);
    if Cmp(JvInterpreterEventDesc.Identifier, EventType) then
    begin
      Event := JvInterpreterEventDesc.EventClass.Create(AOwner, AObject, UnitName, FunName);
      TMethod(Result).Code := JvInterpreterEventDesc.Code;
      TMethod(Result).Data := Event;
      Exit;
    end;
  end;
  Result := nil; { satisfy compiler }
end;

function TJvInterpreterAdapter.IsEvent(Obj: TObject; const Identifier: string): Boolean;
var
  JvInterpreterClass: TJvInterpreterClass;
  i: Integer;
begin
  for i := 0 to FEventList.Count - 1 do
  begin
    JvInterpreterClass := TJvInterpreterClass(FEventList[i]);
    if (Obj is JvInterpreterClass.FClassType) and
      Cmp(JvInterpreterClass.Identifier, Identifier) then
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
  FIntfGetList.Sort;
  FSetList.Sort;
  FIGetList.Sort;
  FISetList.Sort;
  FSorted := True;
end;

//=== TJvInterpreterArgs =====================================================

procedure TJvInterpreterArgs.Clear;
begin
  Count := 0;
  Obj := nil;
  ObjTyp := 0;
  HasVars := False;
  Indexed := False;
  ReturnIndexed := False;
  ObjRefHolder := Unassigned;
end;

destructor TJvInterpreterArgs.Destroy;
begin
  if OA <> nil then
    Dispose(OA);
  if OAV <> nil then
    Dispose(OAV);
  inherited Destroy;
end;

procedure TJvInterpreterArgs.OpenArray(const Index: Integer);
begin
  if OA = nil then
    New(OA);
  if OAV = nil then
    New(OAV);
  V2OA(Values[Index], OA^, OAV^, OAS);
end;

procedure TJvInterpreterArgs.Delete(const Index: Integer);
var
  i: Integer;
begin
  for i := Index to Count - 2 do
  begin
    Types[i] := Types[i + 1];
    Values[i] := Values[i + 1];
    Names[i] := Names[i + 1];
  end;
  Dec(Count);
end;

//=== TJvInterpreterExpression ===============================================

constructor TJvInterpreterExpression.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parser := TJvInterpreterParser.Create;
  FPStream := TStringStream.Create('');
  FArgs := TJvInterpreterArgs.Create;
  FAdapter := CreateAdapter;
  FSharedAdapter := GlobalJvInterpreterAdapter;
  FLastError := EJvInterpreterError.Create(-1, -1, '', '');
  AllowAssignment := True;
end;

destructor TJvInterpreterExpression.Destroy;
begin
  //JvInterpreterVarFree(FVResult);
  FAdapter.Free;
  FArgs.Free;
  FPStream.Free;
  Parser.Free;
  FLastError.Free;
  inherited Destroy;
end;

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
        { first line has number 1 }
        E.FErrLine := GetLineByPos(Parser.Source, E.FErrPos) + BaseErrLine + 1;
        E.Message := Format(LoadStr2(ieErrorPos), [E.FErrUnitName, E.FErrLine, E.FMessage1]);
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
end;

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
end;

function TJvInterpreterExpression.GetSource: string;
begin
  Result := Parser.Source;
end;

procedure TJvInterpreterExpression.SetSource(Value: string);
begin
  Parser.Source := Value;
  SourceChanged;
end;

procedure TJvInterpreterExpression.SourceChanged;
begin
end;

procedure TJvInterpreterExpression.SetAdapter(Adapter: TJvInterpreterAdapter);
begin
  FAdapter := Adapter;
end;

procedure TJvInterpreterExpression.SetCurPos(Value: Integer);
begin
  if FParsed then
    FPStream.Position := Value
  else
    Parser.Pos := Value;
  FBacked := False;
end;

function TJvInterpreterExpression.GetCurPos: Integer;
begin
  if FParsed then
    Result := FPStream.Position
  else
    Result := Parser.Pos;
end;

procedure TJvInterpreterExpression.ErrorExpected(Exp: string);
begin
  if TokenStr <> '' then
    JvInterpreterErrorN2(ieExpected, PosBeg, Exp, '''' + TokenStr + '''')
  else
    JvInterpreterErrorN2(ieExpected, PosBeg, Exp, LoadStr2(irEndOfFile));
end;

procedure TJvInterpreterExpression.ErrorNotImplemented(Message: string);
begin
  JvInterpreterErrorN(ieInternal, PosBeg, Message + ' not implemented');
end;

function TJvInterpreterExpression.PosBeg: Integer;
begin
  Result := CurPos - Length(TokenStr);
end;

function TJvInterpreterExpression.PosEnd: Integer;
begin
  Result := CurPos;
end;

function TJvInterpreterExpression.GetTokenStr: string;
begin
  if FParsed and (TTyp <> ttUnknown) then
    Result := TypToken(TTyp)
  else
    Result := TokenStr1;
end;

procedure TJvInterpreterExpression.Parse;
begin
{$IFNDEF COMPILER2}
  FPStream.Size := 0;
{$ELSE}
  (FPStream as TJvStringStream).SetSize(0);
{$ENDIF}
  FPStream.Position := 0;
  Parser.Init;
  repeat
    ParseToken;
    WriteToken;
  until TTyp1 = ttEmpty;
  FParsed := True;
  FPStream.Position := 0;
end;

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
    ttIdentifier:
      StringSaveToStream(FPStream, Token1);
    ttUnknown:
      StringSaveToStream(FPStream, TokenStr1);
  end;
end;

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
    ttIdentifier:
      Token1 := StringLoadFromStream(FPStream);
    ttUnknown:
      TokenStr1 := StringLoadFromStream(FPStream);
  end;
end;

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
end;

procedure TJvInterpreterExpression.ParseToken;
var
  OldDecimalSeparator: Char;
  Dob: Extended;
  Int: Integer;
  Stub: Integer;
begin
  TokenStr1 := Parser.Token;
  TTyp1 := TokenTyp(TokenStr1);
  case TTyp of
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
    ttIdentifier:
      Token1 := TokenStr1;
    {-----olej-----}
    ttArray:
      Token1 := TokenStr1;
    {-----olej-----}
  end;
end;

procedure TJvInterpreterExpression.Back;
begin
//  JvInterpreterError(ieInternal, -2);
  if FBacked then
    JvInterpreterError(ieInternal, -1);
  FBacked := True;
end;

procedure TJvInterpreterExpression.SafeBack;
begin
  if not FBacked then
    Back;
end;

function TJvInterpreterExpression.CreateAdapter: TJvInterpreterAdapter;
begin
  Result := TJvInterpreterAdapter.Create(Self);
end;

function TJvInterpreterExpression.Expression1: Variant;
var
  OldExpStackPtr: Integer;
  DivZeroException:EJvInterpreterError;

  procedure PushExp(var Value: Variant);
  begin
    Inc(ExpStackPtr);
    if ExpStackPtr > High(ExpStack) then
      JvInterpreterError(ieExpressionStackOverflow, -1);
    JvInterpreterVarCopy(ExpStack[ExpStackPtr], Value);
  end;

  function PopExp: Variant;
  begin
    if ExpStackPtr = -1 then
      JvInterpreterError(ieInternal, -1);
    JvInterpreterVarCopy(Result, ExpStack[ExpStackPtr]);
    Dec(ExpStackPtr);
  end;

  {NEW: Handle division by zero errors cleanly, avoids Runtime Errors. }
  procedure DivByZero;
  begin
      DivZeroException := EJvInterpreterError.Create(ieDivisionByZero, PosEnd, 'Division By Zero','');
      UpdateExceptionPos(DivZeroException, 'DivisionByZero');
      Self.FVResult := NaN; // Use NaN as a DivByZero indicator.
      raise DivZeroException;
  end;
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
      case TTyp of
        ttInteger, ttDouble, ttString, ttFalse, ttTrue, ttIdentifier:
          begin
            Result := Token;
            if TTyp = ttIdentifier then
            begin
              Args.Clear;
              InternalGetValue(nil, 0, Result);
            end;
            NextToken;
            if TTyp in [ttInteger, ttDouble, ttString,
              ttFalse, ttTrue, ttIdentifier] then
              JvInterpreterError(ieMissingOperator, PosEnd {!!});
            if Prior(TTyp) < Prior(OpTyp) then
              Exit;
          end;
        ttMul:
          if priorMul > Prior(OpTyp) then
            Result := PopExp * Expression(TTyp)
          else
            Exit;
        ttPlus:
         { proceed differently depending on a types }
          if not (PrevTTyp in [ttInteger, ttDouble, ttString, ttFalse, ttTrue,
            ttIdentifier, ttRB, ttRS]) then
           { unar plus }
            Result := Expression(ttNot {highest priority})
          else if priorPlus > Prior(OpTyp) then
          begin
            Tmp := PopExp;
            if TVarData(Tmp).VType = varSet then
            begin
              Result := TVarData(Tmp).VInteger or
                TVarData(Expression(TTyp)).VInteger;
              TVarData(Result).VType := varSet;
            end
            else
              Result := Tmp + Expression(TTyp)
          end
          else
            Exit;
        ttMinus:
         { proceed differently depending on a types }
          if not (PrevTTyp in [ttInteger, ttDouble, ttString, ttFalse, ttTrue,
            ttIdentifier, ttRB, ttRS]) then
           { unar minus }
            Result := -Expression(ttNot {highest priority})
          else if priorMinus > Prior(OpTyp) then
          begin
            Tmp := PopExp;
            if TVarData(Tmp).VType = varSet then
            begin
              Result := TVarData(Tmp).VInteger and not
                TVarData(Expression(TTyp)).VInteger;
              TVarData(Result).VType := varSet;
            end
            else
              Result := Tmp - Expression(TTyp)
          end
          else
            Exit;
        ttDiv:
          if priorDiv > Prior(OpTyp) then begin
            Tmp := Expression(TTyp);
            if (VarType(Tmp) = varDouble) then begin
                if (Tmp = 0.0) then DivByZero;
            end else if (VarType(Tmp) = varInteger) then begin
                if (Tmp = 0) then DivByZero;
            end;
            // Now we get here, we know that either
            // the right hand side is nonzero or the types are wrong.
            // If types are wrong the variant division routine will
            // raise a runtime error. If Tmp is zero we would get
            // a low level runtime error, sadly.
            Result := PopExp / Tmp;
          end else
            Exit;
        ttIntDiv:
          if priorIntDiv > Prior(OpTyp) then begin
            Tmp := Expression(TTyp);
            Result := PopExp div Tmp;
          end else
            Exit;
        ttMod:
          if priorMod > Prior(OpTyp) then
            Result := PopExp mod Expression(TTyp)
          else
            Exit;
        ttOr:
          if priorOr > Prior(OpTyp) then
            Result := PopExp or Expression(TTyp)
          else
            Exit;
        ttAnd:
          if priorAnd > Prior(OpTyp) then
            Result := PopExp and Expression(TTyp)
          else
            Exit;
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
            if (TVarData(Tmp).VType = varObject) or (TVarData(Tmp).VType = varClass) or (TVarData(Tmp).VType = varSet) or (TVarData(Tmp).VType = varPointer) then
              Result := TVarData(Tmp).VInteger =
                TVarData(Expression(TTyp)).VInteger
            else
              Result := Tmp = Expression(TTyp)
          end
          else
            Exit;
        ttNotEqu:
          { proceed differently depending on a types }
          if priorNotEqu > Prior(OpTyp) then
          begin
            Tmp := PopExp;
            if (TVarData(Tmp).VType = varObject) or (TVarData(Tmp).VType = varClass) or (TVarData(Tmp).VType = varSet) or (TVarData(Tmp).VType = varPointer) then
              Result := TVarData(Tmp).VInteger <>
                TVarData(Expression(TTyp)).VInteger
            else if TVarData(Tmp).VType = varUnknown  then
              Result := TVarData(Tmp).VUnknown <>
                TVarData(Expression(TTyp)).VUnknown
            else
              Result := Tmp <> Expression(TTyp)
          end
          else
            Exit;
        ttGreater:
          if priorGreater > Prior(OpTyp) then
            Result := PopExp > Expression(TTyp)
          else
            Exit;
        ttLess:
          if priorLess > Prior(OpTyp) then
            Result := PopExp < Expression(TTyp)
          else
            Exit;
        ttEquLess:
          if priorEquLess > Prior(OpTyp) then
            Result := PopExp <= Expression(TTyp)
          else
            Exit;
        ttEquGreater:
          if priorEquGreater > Prior(OpTyp) then
            Result := PopExp >= Expression(TTyp)
          else
            Exit;
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
          else
            Exit;
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
          else
            Exit;
      else
        if TVarData(Result).VType = varEmpty then
          ErrorExpected(LoadStr2(irExpression))
        else
          Exit;
      end;
      PushExp(Result);
    end;
  end;

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
  end;
end;

function TJvInterpreterExpression.Expression2(const ExpType: Word): Variant;
var
  ErrPos: Integer;
begin
  ErrPos := PosBeg;
  try
    AllowAssignment := False;
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
    end;
end;

{ calulate set expressions, such as: [fsBold, fsItalic] }

function TJvInterpreterExpression.SetExpression1: Variant;
var
  V1: Variant;
begin
  Result := 0;
  while True do
  begin
    case TTyp of
      ttIdentifier, ttInteger:
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
end;

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
      Inc(i);
    end;
    if TTyp <> ttRS then
      ErrorExpected(''']''');
    Result := VarArrayCreate([0, i - 1], varVariant);
    for i := 0 to i - 1 do
      Result[i] := Values[i];
    NextToken;
  end;

var
  LocalArgs: TJvInterpreterArgs;
  i: Integer;
  SK: TTokenTyp;
begin
  LocalArgs := Args;
  Args := TJvInterpreterArgs.Create;
  Args.Indexed := LocalArgs.Indexed;
  try
    i := 0;
    if TTyp = ttLB then
      SK := ttRB
    else
      SK := ttRS;

    NextToken;
    if TTyp = ttIdentifier then
      LocalArgs.VarNames[i] := Token
    else
      LocalArgs.VarNames[i] := '';

    Args.Clear;
    if TTyp = ttLS then
      LocalArgs.Values[i] := ReadOpenArray
    //added check to recognize C style (), like "NextToken()"
    //RWare: if token ')', skip and exit
    else if TTyp = ttRB then
    begin
      NextToken;
      Exit;
    end
    else
      JvInterpreterVarCopy(LocalArgs.Values[i], Expression1);

    while TTyp = ttCol do
    begin
      Inc(i);
      NextToken;
      if TTyp = ttIdentifier then
        LocalArgs.VarNames[i] := Token
      else
        LocalArgs.VarNames[i] := '';
      Args.Clear;
      if TTyp = ttLS then
        LocalArgs.Values[i] := ReadOpenArray
      else
        JvInterpreterVarCopy(LocalArgs.Values[i], Expression1);
    end;
    if TTyp <> SK then
      if SK = ttRB then
        ErrorExpected(''')''')
      else
        ErrorExpected(''']''');
    NextToken;
    LocalArgs.Count := i + 1;
  finally
    Args.Free;
    Args := LocalArgs;
  end;
end;

procedure TJvInterpreterExpression.InternalGetValue(Obj: Pointer; ObjTyp: Word;
  var Result: Variant);
var
  Identifier: string;
  V: Variant;
  VType: TVarType;

  procedure UpdateVarParams;
  var
    i, C: Integer;
  begin
    if not Args.HasVars then
      Exit;
    C := Args.Count;
    Args.Obj := nil;
    Args.ObjTyp := 0;
    Args.ObjRefHolder := Unassigned;
    Args.Count := 0;
    for i := 0 to C - 1 do
      if (Args.VarNames[i] <> '') and ((Args.Types[i] and varByRef) <> 0) then
      {  if not }SetValue(Args.VarNames[i], Args.Values[i], Args) {then
          JvInterpreterErrorN(ieUnknownIdentifier, PosBeg, Args.VarNames[i])};
    Args.HasVars := False;
  end;

begin
  Identifier := Token;
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
    if (ObjTyp = varDispatch) then
      Args.ObjRefHolder := IDispatch(Obj)
    else if (ObjTyp = varUnknown) then
      Args.ObjRefHolder := IUnknown(Obj);

    Back;
    Token1 := Identifier; {!!!!!!!!!!!!!!}
    { Args.Obj, Args.ObjTyp, Args.Count needed in caller }
    Exit;
  end;

  { need result if object field or method or assignment }
  Args.HasResult := (TTyp in [ttPoint, ttRB, ttCol, ttNot..ttEquLess]) or
    Args.Assignment;
  Args.ReturnIndexed := False;

  if GetValue(Identifier, Result, Args) then
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
      JvInterpreterVarCopy(V, Result);
      if not GetElement(V, Result, Args) then
        { problem }
        JvInterpreterError(ieArrayRequired, PosBeg);
    end;
  end
  else
    JvInterpreterErrorN(ieUnknownIdentifier, PosBeg {?}, Identifier);

  Args.Obj := nil;
  Args.ObjTyp := 0;
  Args.ObjRefHolder := Unassigned;
  Args.Count := 0;
  { Args.Obj, Args.ObjTyp, Args.Count NOT needed in caller }

  if TTyp = ttPoint then { object field or method }
  begin
    NextToken;
    if TTyp <> ttIdentifier then
      ErrorExpected(LoadStr2(irIdentifier));
    VType := TVarData(Result).VType;
    if (VType <> varObject) and
      (VType <> varClass) and
      (VType <> varRecord) and
      (VType <> varDispatch) and
      (VType <> varUnknown) then
    {if not (TVarData(Result).VType in
      [varObject, varClass, varRecord, varDispatch, varUnknown]) then}
      JvInterpreterError(ieROCRequired, PosBeg);

    V := Null;
    InternalGetValue(TVarData(Result).vPointer, TVarData(Result).VType, V);
    JvInterpreterVarCopy(Result, V);

    NextToken;
  end;

  Back;
end;

function TJvInterpreterExpression.GetElement(const Variable: Variant; var Value: Variant;
  var Args: TJvInterpreterArgs): Boolean;
var
  II2: Integer;
  VV: TJvInterpreterArrayValues;
  PP: PJvInterpreterArrayRec;
  Bound: Integer;
  AI: array of integer;
begin
  Result := False;
  if Args.Count <> 0 then
  begin
    if TVarData(Variable).VType = varString then
    begin
      if Args.Count > 1 then
        JvInterpreterError(ieArrayTooManyParams, -1);
      if Length(Variable) = 0 then
        raise ERangeError.Create('range check error');
      Value := string(Variable)[Integer(Args.Values[0])];
      Result := True;
    end
    else if TVarData(Variable).VType = varArray then
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
    end
    else if (TVarData(Variable).VType = varObject) or
    (TVarData(Variable).VType = varClass) then
    begin
      Result := FAdapter.GetElement(Self, Variable, Value, Args);
      if not Result and Assigned(FSharedAdapter) then
        Result := FSharedAdapter.GetElement(Self, Variable, Value, Args);
    end
    {For Variant Arrays}
    {$IFDEF COMPILER6_UP}
    // No support for variant arrays on Delphi 5 yet, sorry
    else if VarIsArray(Variable) then
    begin
      if Args.Count > VarArrayDimCount(Variable) then
        JvInterpreterError(ieArrayTooManyParams, -1)
      else if Args.Count < VarArrayDimCount(Variable) then
        JvInterpreterError(ieArrayNotEnoughParams, -1);
      AI := nil;
      SetLength(AI, Args.Count);
      for II2 := 0 to Args.Count - 1 do
      begin
        Bound := Args.Values[II2];
        if Bound > VarArrayHighBound(Variable, II2 +1) then
          JvInterpreterError(ieArrayIndexOutOfBounds, -1);
        if Bound < VarArrayLowBound(Variable, II2 +1) then
          JvInterpreterError(ieArrayIndexOutOfBounds, -1);
        AI[II2] := Bound;
      end;
      Value := VarArrayGet(Variable, AI);
      Result := True;
    end
    {$ENDIF COMPILER6_UP}
    else
      { problem }
      JvInterpreterError(ieArrayRequired, CurPos);

  end;
end;

function TJvInterpreterExpression.SetElement(var Variable: Variant; const Value: Variant;
  var Args: TJvInterpreterArgs): Boolean;
var
  II2: Integer;
  VV: TJvInterpreterArrayValues;
  PP: PJvInterpreterArrayRec;
  Bound: Integer;
  AI: array of integer;
begin
  Result := False;
  if Args.Count <> 0 then
  begin
    if TVarData(Variable).VType = varString then
    begin
      if Args.Count > 1 then
        JvInterpreterError(ieArrayTooManyParams, -1);
      string(TVarData(Variable).vString)[Integer(Args.Values[0])] := string(Value)[1];
      Result := True;
    end
    else if TVarData(Variable).VType = varArray then
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
    end
    else if (TVarData(Variable).VType = varObject) or
    (TVarData(Variable).VType = varClass) then
    begin
      Result := FAdapter.SetElement(Self, Variable, Value, Args);
      if not Result and Assigned(FSharedAdapter) then
        Result := FSharedAdapter.SetElement(Self, Variable, Value, Args);
    end
    {For Variant Array}
    {$IFDEF COMPILER6_UP}
    // No support for variant arrays on Delphi 5 yet, sorry
    else if VarIsArray(Variable) then
    begin
      if Args.Count > VarArrayDimCount(Variable) then
        JvInterpreterError(ieArrayTooManyParams, -1)
      else if Args.Count < VarArrayDimCount(Variable) then
        JvInterpreterError(ieArrayNotEnoughParams, -1);
      AI := nil;
      SetLength(AI, Args.Count);
      for II2 := 0 to Args.Count - 1 do
      begin
        Bound := Args.Values[II2];
        if Bound > VarArrayHighBound(Variable, II2 +1) then
          JvInterpreterError(ieArrayIndexOutOfBounds, -1);
        if Bound < VarArrayLowBound(Variable, II2 +1) then
          JvInterpreterError(ieArrayIndexOutOfBounds, -1);
        AI[II2] := Bound;
      end;
      VarArrayPut(Variable, Value, AI);
      Result := True;
    end
    {$ENDIF COMPILER6_UP}
    else
      { problem }
      JvInterpreterError(ieArrayRequired, CurPos);

  end;
end;

function TJvInterpreterExpression.GetValue(Identifier: string; var Value: Variant;
  var Args: TJvInterpreterArgs): Boolean;
begin
  try
    Result := FAdapter.GetValue(Self, Identifier, Value, Args);
    if not Result and Assigned(FSharedAdapter) then
      Result := FSharedAdapter.GetValue(Self, Identifier, Value, Args);
  except
    on E: Exception do
    begin
      UpdateExceptionPos(E, '');
      raise;
    end;
  end;
  if not Result and Assigned(FOnGetValue) then
    FOnGetValue(Self, Identifier, Value, Args, Result);
end;

function TJvInterpreterExpression.SetValue(Identifier: string; const Value: Variant;
  var Args: TJvInterpreterArgs): Boolean;
begin
  try
    Result := FAdapter.SetValue(Self, Identifier, Value, Args);
    if not Result and Assigned(FSharedAdapter) then
      Result := FSharedAdapter.SetValue(Self, Identifier, Value, Args);
  except
    on E: EJvInterpreterError do
    begin
      E.FErrPos := PosBeg;
      raise;
    end;
  end;
  if not Result and Assigned(FOnSetValue) then
    FOnSetValue(Self, Identifier, Value, Args, Result);
end;

procedure TJvInterpreterExpression.Run;
begin
  Init;
  NextToken;
  FVResult := Expression1;
end;

//=== TJvInterpreterFunction =================================================

constructor TJvInterpreterFunction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FunStack := TList.Create;
  SS := TStringList.Create;
  FEventList := TList.Create;
end;

destructor TJvInterpreterFunction.Destroy;
begin
  SS.Free;
  FunStack.Free;
  ClearList(FEventList);
  FEventList.Free;
  inherited Destroy;
end;

procedure TJvInterpreterFunction.Init;
begin
  inherited Init;
  FBreak := False;
  FContinue := False;
  FunStack.Clear;
  StateStackPtr := -1;
  FCurUnitName := '';
  FCurInstance := nil;
end;

procedure TJvInterpreterFunction.PushState;
begin
  Inc(StateStackPtr);
  if StateStackPtr > High(StateStack) then
    JvInterpreterError(ieInternal, -1);
  StateStack[StateStackPtr].Token := Token1;
  StateStack[StateStackPtr].TTyp := TTyp1;
  StateStack[StateStackPtr].PrevTTyp := PrevTTyp;
  StateStack[StateStackPtr].Backed := FBacked;
  StateStack[StateStackPtr].CurPos := CurPos;
  StateStack[StateStackPtr].AllowAssignment := AllowAssignment;
end;

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
  Dec(StateStackPtr);
end;

procedure TJvInterpreterFunction.RemoveState;
begin
  Dec(StateStackPtr);
end;

function TJvInterpreterFunction.GetLocalVars: TJvInterpreterVarList;
begin
  if FunContext <> nil then
    Result := PFunContext(FunContext).LocalVars
  else
    Result := nil;
end;

procedure TJvInterpreterFunction.InFunction1(FunDesc: TJvInterpreterFunDesc);
var
  FunArgs: TJvInterpreterArgs;
  VarNames: PNameArray;

  procedure EnterFunction;
  var
    FC: PFunContext;
    i: Integer;
    v: variant;
  begin
    New(PFunContext(FC));
    FillChar(FC^, SizeOf(FC^), 0);
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
      for i := 0 to Args.Count - 1 do
      begin
        if (FunDesc.FParamTypes[i] and varByRef) <> 0 then
          JvInterpreterVarCopy(v, Args.Values[i])
        else
          JvInterpreterVarAssignment(v, Args.Values[i]);

        PFunContext(FunContext).LocalVars.AddVar('', FunDesc.FParamNames[i], '',
          FunDesc.FParamTypes[i], v,
          TJvInterpreterSimpleDataType.Create(FunDesc.FParamTypes[i]));
        VarNames^ := FunDesc.FParamNames;
        Args.HasVars := Args.HasVars or ((FunDesc.FParamTypes[i] and varByRef)
          <> 0);
      end;
      if FunDesc.ResTyp > 0 then
      begin
        FunDesc.ResDataType.Init(v);
        PFunContext(FunContext).LocalVars.AddVar('', 'Result', '',
          FunDesc.ResTyp, v, FunDesc.ResDataType);
      end
    end
    else
      PFunContext(FunContext).LocalVars.AddVar('', 'Result', '', varVariant,
        Null, TJvInterpreterSimpleDataType.Create(varVariant));
    FunArgs := Args;
    Args := TJvInterpreterArgs.Create;
  end;

  procedure LeaveFunction(Ok: Boolean);
  var
    FC: PFunContext;
    C: Integer;

    procedure UpdateVarParams;
    var
      i, C: Integer;
    begin
      if not Args.HasVars then
        Exit;
      C := Args.Count;
      Args.Obj := nil;
      Args.ObjTyp := 0;
      Args.ObjRefHolder := Unassigned;
      Args.Count := 0;

      for i := 0 to C - 1 do { Iterate }
        if (VarNames[i] <> '') and
          ((Args.Types[i] and varByRef) <> 0) then
          GetValue(VarNames[i], Args.Values[i], Args);
    end; { SetVarParams }

  begin
    Args.Free;
    Args := FunArgs;
    if Ok then
    begin
      C := Args.Count;
      UpdateVarParams;
      Args.Count := 0;
      if (FunDesc = nil) or (FunDesc.ResTyp > 0) then
      begin
        PFunContext(FunContext).LocalVars.GetValue('Result', FVResult, Args);
        TVarData(PFunContext(FunContext).LocalVars.FindVar('', 'Result').Value).VType := varEmpty;
        TVarData(PFunContext(FunContext).LocalVars.FindVar('', 'Result').Value).VPointer := nil;
      end;

      Args.Count := C;
    end;
    FC := PFunContext(FunContext).PrevFunContext;
    PFunContext(FunContext).LocalVars.Free;
    Dispose(PFunContext(FunContext));
    Dispose(VarNames);
    FunStack.Delete(FunStack.Count - 1);
    FunContext := FC;
  end;

begin
  { allocate stack }
  EnterFunction;
  try
    FExit := False;
    while True do
    begin
      case TTyp of
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
      end;
      NextToken;
    end;
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
  end;
end;

function TJvInterpreterFunction.GetValue(Identifier: string; var Value: Variant;
  var Args: TJvInterpreterArgs): Boolean;
begin
  Result := False;
  { check in local variables }
  try
    if FunContext <> nil then
      Result := PFunContext(FunContext).LocalVars.GetValue(Identifier, Value, Args);
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
    Result := inherited GetValue(Identifier, Value, Args);
end;

function TJvInterpreterFunction.SetValue(Identifier: string; const Value: Variant;
  var Args: TJvInterpreterArgs): Boolean;
begin
  Result := False;
  { check in local variables }
  try
    if FunContext <> nil then
      Result := PFunContext(FunContext).LocalVars.SetValue(Identifier, Value, Args);
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
    Result := inherited SetValue(Identifier, Value, Args);
end;

procedure TJvInterpreterFunction.DoOnStatement;
begin
end;

{ exit: current position set to next token }

procedure TJvInterpreterFunction.Statement1;
begin
  DoOnStatement;
  case TTyp of
    ttIdentifier:
      { assignment or function call }
      begin
        Identifier1;
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
  end;
end;

{ exit: current position set to next token }
{ very simple version, many syntax errors are not found out }

procedure TJvInterpreterFunction.SkipStatement1;
begin
  case TTyp of
    ttEmpty:
      ErrorExpected('''' + kwEND + '''');
    ttIdentifier:
      SkipIdentifier1;
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
        SkipIdentifier1;
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
        SkipIdentifier1;
      end;
    ttExit:
      NextToken;
    ttCase:
      begin
        SkipToEnd1;
        Exit;
      end;
  end;
end;

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
end;

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
end;

{exit: current position set to next token after assignment or function call }

procedure TJvInterpreterFunction.SkipIdentifier1;
begin
  while True do
    case TTyp of
      ttEmpty:
        ErrorExpected('''' + kwEND + '''');
      ttIdentifier..ttBoolean, ttLB, ttRB, ttCol, ttPoint, ttLS, ttRS,
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
    end;
end;

procedure TJvInterpreterFunction.FindToken1(TTyp1: TTokenTyp);
begin
  while not (TTyp in [TTyp1, ttEmpty]) do
    NextToken;
  if TTyp = ttEmpty then
    ErrorExpected('''' + kwEND + '''');
end;

function TJvInterpreterFunction.NewEvent(const UnitName: string; const FunName,
  EventType: string; Instance: TObject): TSimpleEvent;
begin
  Result := FAdapter.NewEvent(UnitName, FunName, EventType, Self, Instance);
  if not Assigned(Result) then
    Result := GlobalJvInterpreterAdapter.NewEvent(UnitName, FunName, EventType, Self, Instance);
  if not Assigned(Result) then
    JvInterpreterErrorN(ieEventNotRegistered, -1, EventType);
end;

procedure TJvInterpreterFunction.InternalSetValue(const Identifier: string);
var
  FunDesc: TJvInterpreterFunDesc;
  PropInf: PPropInfo;
  FunName: string;
  PopSt: Boolean;
  MyArgs: TJvInterpreterArgs;
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
          PropInf := GetPropInfo(Args.Obj.ClassInfo, Identifier);
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
          else if FAdapter.IsEvent(Args.Obj, Identifier) then { check only local adapter }
          begin
            if not SetValue(Identifier, FunName, Args) then
              JvInterpreterErrorN(ieUnknownIdentifier, PosBeg, Identifier);
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
  Args := TJvInterpreterArgs.Create;
  try
    Args.Assignment := True;
    JvInterpreterVarCopy(FVResult, Expression1);
  finally
    { pop args }
    Args.Free;
    Args := MyArgs;
  end;
  if Args.Indexed then
  begin
    MyArgs := TJvInterpreterArgs.Create;
    try
      if GetValue(Identifier, Variable, MyArgs) then
      begin
        if not SetElement(Variable, FVResult, Args) then
          { problem }
          JvInterpreterError(ieArrayRequired, PosBeg);
        if (TVarData(Variable).VType = varString) and
          not SetValue(Identifier, Variable, MyArgs) then
          JvInterpreterErrorN(ieUnknownIdentifier, PosBeg, Identifier);
        if VarIsArray(Variable) and
          not SetValue(Identifier, Variable, MyArgs) then
          JvInterpreterErrorN(ieUnknownIdentifier, PosBeg, Identifier);
      end
      else if not SetValue(Identifier, FVResult, Args) then
        JvInterpreterErrorN(ieUnknownIdentifier, PosBeg, Identifier);
    finally
      MyArgs.Free;
    end;
  end
  else if not SetValue(Identifier, FVResult, Args) then
    JvInterpreterErrorN(ieUnknownIdentifier, PosBeg, Identifier);
end;

procedure TJvInterpreterFunction.Identifier1;
var
  Identifier: string;
begin
  Identifier := Token;
  Args.Clear;
  NextToken;
  if TTyp <> ttColon then
  begin
    Back;
    Args.Assignment := False;
    InternalGetValue(nil, 0, FVResult);
    Identifier := Token; { Back! }
    NextToken;
  end;
  if TTyp = ttColon then { assignment }
  begin
    NextToken;
    if TTyp <> ttEqu then
      ErrorExpected('''=''');
    NextToken;
    InternalSetValue(Identifier);
  end;
end;

{exit: current position set to next token after "end"}

procedure TJvInterpreterFunction.Begin1;
begin
  NextToken;
  while True do
  begin
    case TTyp of
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
      ttIdentifier, ttBegin, ttIf, ttWhile, ttFor, ttRepeat,
        ttBreak, ttContinue, ttTry, ttRaise, ttExit, ttCase:
        Statement1;
    else
      ErrorExpected('''' + kwEND + '''');
    end;
    if FBreak or FContinue or FExit then
      Exit;
  end;
end;

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
end;

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
    end;
  finally
    PopState;
  end;
  SkipStatement1;
  FContinue := False;
  FBreak := False;
end;

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
      ttIdentifier, ttBegin, ttIf, ttWhile, ttFor, ttRepeat,
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
  end;
  if FBreak or FExit then
  begin
    SkipToUntil1;
    SkipIdentifier1;
  end;
  FContinue := False;
  FBreak := False;
end;

{exit: current position set to next token after loop }

procedure TJvInterpreterFunction.For1;
var
  i: Integer;
  DoCurPos: Integer;
  iBeg, iEnd: Integer;
  LoopVariable: string;
  ForwardDirection: Boolean;
begin
  NextToken;
  if TTyp <> ttIdentifier then
    ErrorExpected(LoadStr2(irIdentifier));
  // CheckLocalIdentifier;
  LoopVariable := Token;
  NextToken;
  if TTyp <> ttColon then
    ErrorExpected(''':''');
  NextToken;
  if TTyp <> ttEqu then
    ErrorExpected('''=''');
  NextToken;
  iBeg := Expression2(varInteger);
  if (TTyp <> ttTo) and (TTyp <> ttDownTo) then
    ErrorExpected('''' + kwTO + ''' or ''' + kwDOWNTO + '''');
  ForwardDirection := TTyp = ttTo;

  NextToken;
  iEnd := Expression2(varInteger);
  if TTyp <> ttDo then
    ErrorExpected('''' + kwDO + '''');
  DoCurPos := CurPos;
  NextToken;

  if ForwardDirection then
  begin
    for i := iBeg to iEnd do
    begin
      Args.Clear;
      if not SetValue(LoopVariable, i, Args) then
        JvInterpreterErrorN(ieUnknownIdentifier, PosBeg, LoopVariable);
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
    end;
  end else
  begin
    for i := iBeg downto iEnd do
    begin
      Args.Clear;
      if not SetValue(LoopVariable, i, Args) then
        JvInterpreterErrorN(ieUnknownIdentifier, PosBeg, LoopVariable);
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
    end;
  end;

  SkipStatement1;
  FContinue := False;
  FBreak := False;
end;

{exit: current position set to next token after case }

procedure TJvInterpreterFunction.Case1;
var
  Selector, Expression, i: integer;
  EArray: array of array [0..1] of integer;

  function InCase(CaseSel: integer): boolean;
  var
    i: integer;
  begin
    Result := false;
    for i := 0 to length(EArray) - 1 do
     if (CaseSel >= EArray[i][0]) and (CaseSel <= EArray[i][1]) then
     begin
       Result := true;
       Exit;
     end;
  end;

begin
  NextToken;
  Selector := Expression2(varInteger);
  if TTyp <> ttOf then
    ErrorExpected('''' + kwOF + '''');
  while True do
  begin
    NextToken;
    case TTyp of
      ttIdentifier, ttInteger:
        begin
          EArray := nil;
          SetLength(EArray, 1);
          i := 0;
          while true do
          begin
            Expression := Expression2(varInteger);
            EArray[Length(EArray) - 1][i] := Expression;
            if TTyp = ttDoublePoint then
              i := 1
            else
            if TTyp = ttCol then
            begin
              if i = 0 then
                EArray[Length(EArray) - 1][1] := Expression
              else
                i := 0;
              SetLength(EArray, Length(EArray) + 1);
            end
            else
            begin
              if i = 0 then
                EArray[Length(EArray) - 1][1] := Expression;
              break;
            end;
            NextToken;
          end;
          if TTyp <> ttColon then
            ErrorExpected('''' + ':' + '''');
          NextToken;
          if InCase(Selector) then
          begin
            EArray := nil;
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
        begin
          NextToken;
          Break;
        end;
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
//  Typ: Word;
  DT: IJvInterpreterDataType;
  {----olej----}
  {Temporary for array type}
//  ArrayType: Integer;
//  Dimension: Integer;
  {----olej----}
begin
  repeat
    SS.Clear;
    repeat
      NextToken;
      if TTyp <> ttIdentifier then
        ErrorExpected(LoadStr2(irIdentifier));
      SS.Add(Token);
      NextToken;
    until TTyp <> ttCol;
    if TTyp <> ttColon then
      ErrorExpected(''':''');
    NextToken;
    TypName := Token;
    DT := ParseDataType;
    for i := 0 to SS.Count - 1 do
    begin
      DT.Init(Value);
      AddVarFunc(FCurUnitName, SS[i], TypName, DT.GetTyp, Value, DT);
    end;
    SS.Clear;
    NextToken;
    if TTyp <> ttSemicolon then
      ErrorExpected(''';''');
    NextToken;
    Back;
  until TTyp <> ttIdentifier;
end;

procedure TJvInterpreterFunction.Const1(AddVarFunc: TJvInterpreterAddVarFunc);
var
  Identifier: string;
  Value: Variant;
begin
  repeat
    NextToken;
    if TTyp <> ttIdentifier then
      ErrorExpected(LoadStr2(irIdentifier));
    Identifier := Token;
    NextToken;
    if TTyp <> ttEqu then
      ErrorExpected('=');
    NextToken;
    Value := Expression1;

    AddVarFunc(FCurUnitName, Identifier, '', varEmpty, Value,
      TJvInterpreterSimpleDataType.Create(VarType(Value)));
    if TTyp <> ttSemicolon then
      ErrorExpected(''';''');
    NextToken;
    Back;
  until TTyp <> ttIdentifier;
end;

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
  end;

  procedure Except1(E: Exception);
  var
    ExceptionClassName, ExceptionVarName: string;
    ExceptionClass: TClass;
    V: Variant;

    function On1: Boolean;
    begin
      NextToken;
      if TTyp <> ttIdentifier then
        ErrorExpected(LoadStr2(irIdentifier));
      ExceptionClassName := Token;
      NextToken;
      if TTyp = ttColon then
      begin
        NextToken;
        if TTyp <> ttIdentifier then
          ErrorExpected(LoadStr2(irIdentifier));
        ExceptionVarName := ExceptionClassName;
        ExceptionClassName := Token;
        NextToken;
      end;
      Args.Clear;
      if not GetValue(ExceptionClassName, V, Args) then
        JvInterpreterErrorN(ieUnknownIdentifier, PosBeg {?}, ExceptionClassName);
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
          ExceptionClassName, varObject, O2V(E),
          TJvInterpreterSimpleDataType.Create(varObject));
        try
          Statement1;
        finally
          PFunContext(FunContext).LocalVars.DeleteVar('', ExceptionVarName);
        end;
        SkipToEnd1;
      end
      else
      begin
        NextToken;
        SkipStatement1;
       { if TTyp = ttSemicolon then
          NextToken; }
      end;
    end;

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
        case TTyp of
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
        end;
      end;
    end
    else
    begin
      Back;
      Begin1;
    end;
  end;

  procedure DoFinallyExcept(E: Exception);
  begin
    case TTyp of
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
  end;

begin
  while True do
  begin
    NextToken;
    case TTyp of
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
      ttIdentifier, ttBegin, ttIf, ttWhile, ttFor, ttRepeat,
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
          end;
        end;
    else
      ErrorExpected('''' + kwFINALLY + '''');
    end;
  end;
end;

procedure TJvInterpreterFunction.Raise1;
var
  V: Variant;
begin
  NextToken;
  case TTyp of
    ttEmpty, ttSemicolon, ttEnd, ttBegin, ttElse, ttFinally, ttExcept:
      { re-raising exception }
      raise EJvInterpreterError.Create(ieRaise, PosBeg, '', '');
    ttIdentifier:
      begin
        InternalGetValue(nil, 0, V);
        if VarType(V) <> varObject then
          JvInterpreterError(ieClassRequired, PosBeg {?});
        raise V2O(V);
      end;
  else
    JvInterpreterError(ieClassRequired, PosBeg {?});
  end;
end;

procedure TJvInterpreterFunction.Run;
begin
  Init;
  NextToken;
  InFunction1(nil);
end;

function TJvInterpreterFunction.GetFunStackCount: Integer;
begin
  Result := FunStack.Count;
end;

function TJvInterpreterFunction.ParseDataType: IJvInterpreterDataType;
var
  TypName: string;
  Typ: Word;
  ArrayBegin, ArrayEnd: TJvInterpreterArrayValues;
  TempBegin, TempEnd: Integer;
  ArrayType: Integer;
  Dimension: Integer;
  Minus1, Minus2: Boolean;
  //
  JvInterpreterRecord: TJvInterpreterRecord;
  ArrayDT: IJvInterpreterDataType;
begin
  //NextToken;
  TypName := Token;
  Dimension := 0;
  if TTyp = ttIdentifier then
  begin
    Typ := TypeName2VarTyp(TypName);
    JvInterpreterRecord := TJvInterpreterRecord(FAdapter.GetRec(TypName));
    if JvInterpreterRecord = nil then
      JvInterpreterRecord := TJvInterpreterRecord(GlobalJvInterpreterAdapter.GetRec(TypName));
    if JvInterpreterRecord <> nil then
      Result := TJvInterpreterRecordDataType.Create(JvInterpreterRecord)
    else
      Result := TJvInterpreterSimpleDataType.Create(Typ);
  end
  else if TTyp = ttArray then
  begin
    {Get Array variables params}
    {This is code is not very clear}
//    Typ := varArray;
    NextToken;
    if (TTyp <> ttLs) and (TTyp <> ttOf) then
      ErrorExpected('''[''' +  ' or ' + '''Of''');
    {Parse Array Range}
    if TTyp = ttLs then
    begin
      Dimension := 0;
      repeat
        NextToken;
        Minus1 := False;
        if (Trim(TokenStr1) = '-') then
        begin
          Minus1 := True;
          NextToken;
        end;
        TempBegin := StrToInt(TokenStr1);
        try
          ArrayBegin[Dimension] := TempBegin;
          if Minus1 then
            ArrayBegin[Dimension] := ArrayBegin[Dimension] * (-1);
        except
          ErrorExpected('''Integer Value''');
        end;

        NextToken;
        if  TTyp <> ttDoublePoint then
          ErrorExpected('''..''');

        NextToken;
        Minus2 := False;
        if (Trim(TokenStr1) = '-') then
        begin
          Minus2 := True;
          NextToken;
        end;
        TempEnd := StrToInt(TokenStr1);
        try
          ArrayEnd[Dimension] := TempEnd;
        except
          if Minus2 then
            ArrayEnd[Dimension] := ArrayEnd[Dimension] * (-1);
          ErrorExpected('''Integer Value''');
        end;

        if (Dimension < 0) or (Dimension > cJvInterpreterMaxArgs) then
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
    end
    else if TTyp = ttOf then
    begin
      Dimension := 1;
      ArrayBegin[0] := 0;
      ArrayEnd[0] := -1;
    end;
      NextToken;
      ArrayType := TypeName2VarTyp(Token);
    //recursion for arrays
    ArrayDT := ParseDataType;

    Result := TJvInterpreterArrayDataType.Create(ArrayBegin, ArrayEnd, Dimension, ArrayType, ArrayDT);
    {end: var A:array [1..200] of Integer, parsing}
  end
  else
    ErrorExpected(LoadStr2(irIdentifier));
end;

//=== TJvInterpreterUnit =====================================================

constructor TJvInterpreterUnit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClearUnits := True;
  FEventHandlerList := TList.Create;
end;

destructor TJvInterpreterUnit.Destroy;
begin
  ClearList(FEventHandlerList);
  FEventHandlerList.Free;
  inherited Destroy;
end;

procedure TJvInterpreterUnit.Init;
begin
  inherited Init;
  if FClearUnits then
  begin
    FAdapter.ClearSource;
    FUnitSection := usUnknown;
    ClearList(FEventHandlerList);
  end;
end;

procedure TJvInterpreterUnit.ReadFunHeader(FunDesc: TJvInterpreterFunDesc);
var
  TypName: string;
  Fun: Boolean;

  procedure ReadParams;
  var
    VarParam: Boolean;
    ParamType: string;
    iBeg: Integer;
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
          ttIdentifier:
            FunDesc.FParamNames[FunDesc.ParamCount] := Token;
          ttSemicolon: Break;
          ttRB: Exit;
          ttColon:
            begin
              NextToken;
              if TTyp <> ttIdentifier then
                ErrorExpected(LoadStr2(irIdentifier));
              ParamType := Token;
              while True do
              begin
                if TTyp = ttRB then
                  Back;
                if TTyp in [ttRB, ttSemicolon] then
                  Break;
                NextToken;
              end;
              Inc(FunDesc.FParamCount);
              while iBeg < FunDesc.FParamCount do
              begin
                FunDesc.FParamTypes[iBeg] := TypeName2VarTyp(ParamType);
                if VarParam then
                  FunDesc.FParamTypes[iBeg] := FunDesc.FParamTypes[iBeg] or
                    varByRef;
                Inc(iBeg);
              end;
              Break;
            end;
          ttCol:
            Inc(FunDesc.FParamCount);
        end;
        NextToken;
      end;
    end;
  end;

begin
  Fun := TTyp = ttFunction;
  NextToken;
  if TTyp <> ttIdentifier then
    ErrorExpected(LoadStr2(irIdentifier));
  FunDesc.FIdentifier := Token;
  NextToken;
  if TTyp = ttPoint then
  begin
    FunDesc.FClassIdentifier := FunDesc.FIdentifier;
    NextToken;
    if TTyp <> ttIdentifier then
      ErrorExpected(LoadStr2(irIdentifier));
    FunDesc.FIdentifier := Token;
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
      if TTyp <> ttIdentifier then
        ErrorExpected(LoadStr2(irIdentifier));
      TypName := Token;
      FunDesc.FResDataType := ParseDataType;
      FunDesc.FResTyp := FunDesc.FResDataType.GetTyp;
      if FunDesc.FResTyp = 0 then
        FunDesc.FResTyp := varVariant;
      NextToken;
    end
    else
      ErrorExpected(''':''');
  if TTyp <> ttSemicolon then
    ErrorExpected(''';''');
end;

procedure TJvInterpreterUnit.Function1;
var
  FunDesc: TJvInterpreterFunDesc;
  FunName: string;
  FunIndex: Integer;
  DllName: string;
  LastTTyp: TTokenTyp; // Ivan_ra
begin
  FunDesc := TJvInterpreterFunDesc.Create;
  try
    ReadFunHeader(FunDesc);
    FunDesc.FPosBeg := CurPos;
    LastTTyp := TTyp; // Ivan_ra
    NextToken;
    if TTyp = ttExternal then
    begin
      NextToken;
      if TTyp = ttString then
        DllName := Token
      else if TTyp = ttIdentifier then
      begin
        Args.Clear;
        if not GetValue(Token, FVResult, Args) then
          JvInterpreterErrorN(ieUnknownIdentifier, PosBeg, Token);
        DllName := vResult;
      end
      else
        ErrorExpected('''string constant'''); {DEBUG!!!}
      NextToken;
      if TTyp <> ttIdentifier then
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
      else if Cmp(Token, 'index') then
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
        FAdapter.AddExtFun(FCurUnitName {??!!}, FIdentifier, noInstance, DllName,
          FunName, FunIndex, FParamCount, FParamTypes, FResTyp);
      NextToken;
    end
    // Ivan_ra start
    else if FUnitSection = usInterface then
    begin
      CurPos := FunDesc.FPosBeg;
      TTyp1 := LastTTyp;
    end
    // Ivan_ra finish
    else
    begin
      FindToken1(ttBegin);
      SkipToEnd1;
      with FunDesc do
        FAdapter.AddSrcFun(FCurUnitName {??!!}, FIdentifier, FPosBeg, CurPos,
          FParamCount, FParamTypes, FParamNames, FResTyp, FResDataType, nil);
    end;
  finally
    FunDesc.Free;
  end;
end;

procedure TJvInterpreterUnit.ReadUnit(const UnitName: string);
var
  OldUnitName: string;
  OldSource: string;
  S: string;
begin
  if FAdapter.UnitExists(UnitName) then
    Exit;
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
  finally
    FCurUnitName := OldUnitName;
    Source := OldSource;
    PopState;
  end;
end;

procedure TJvInterpreterUnit.Uses1(var UsesList: string);
begin
  NextToken;
  if not (TTyp in [ttIdentifier, ttString]) then
    ErrorExpected(LoadStr2(irIdentifier));
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
    if not (TTyp in [ttIdentifier, ttString]) then
      ErrorExpected(LoadStr2(irIdentifier));
    UsesList := UsesList + ',';
    ReadUnit(Token);
  end;
end;

procedure TJvInterpreterUnit.Unit1;
var
  UsesList: string;
begin
  NextToken;
  if TTyp <> ttIdentifier then
    ErrorExpected(LoadStr2(irIdentifier));
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
    end;
    NextToken;
  end;
  if TTyp <> ttEnd then
    ErrorExpected('''' + kwEND + '''');
  NextToken;
  if TTyp <> ttPoint then
    ErrorExpected('''.''');
  FAdapter.AddSrcUnit(FCurUnitName, Source, UsesList);
end;

procedure TJvInterpreterUnit.Type1;
var
  Identifier: string;
begin
  NextToken;
  if TTyp <> ttIdentifier then
    ErrorExpected(LoadStr2(irIdentifier));
  Identifier := Token;
  NextToken;
  if TTyp <> ttEqu then
    ErrorExpected('''=''');
  NextToken;
  case TTyp of
    ttClass:
      Class1(Identifier);
    ttRecord:
      Record1(Identifier);
  else
    { only class declaration for form is supported }
    ErrorExpected(LoadStr2(irClass));
  end;
end;

procedure TJvInterpreterUnit.Class1(const Identifier: string);
var
  JvInterpreterSrcClass: TJvInterpreterIdentifier;
begin
  NextToken;
  if TTyp <> ttLB then
    ErrorExpected('''(''');
  NextToken;
  if TTyp <> ttIdentifier then
    ErrorExpected(LoadStr2(irIdentifier));
  NextToken;
  if TTyp <> ttRB then
    ErrorExpected(''')''');
  FindToken1(ttEnd);
  NextToken;
  if TTyp <> ttSemicolon then
    ErrorExpected(''';''');
  JvInterpreterSrcClass := TJvInterpreterIdentifier.Create;
  JvInterpreterSrcClass.UnitName := FCurUnitName;
  JvInterpreterSrcClass.Identifier := Identifier;
  FAdapter.AddSrcClass(JvInterpreterSrcClass);
end;

procedure TJvInterpreterUnit.Record1(const Identifier: string);
var
//  JvInterpreterSrcRecord: TJvInterpreterIdentifier;
//  Fields: array of TJvInterpreterRecField;
//  TempField: TJvInterpreterRecField;
//  TempCount: integer;
//  TempTyp: Word;
  JvInterpreterRecord: TJvInterpreterRecord;
begin
  JvInterpreterRecord := TJvInterpreterRecord.Create;
  JvInterpreterRecord.RecordSize := 0;
  JvInterpreterRecord.Identifier := Identifier;
  JvInterpreterRecord.FieldCount := 0;
  Var1(JvInterpreterRecord.AddField);
  NextToken;
  if TTyp <> ttEnd then
    ErrorExpected('''' + kwEND + '''');
  NextToken;
  if TTyp <> ttSemicolon then
    ErrorExpected(''';''');
  // конец разбора

  FAdapter.FRecList.Add(JvInterpreterRecord);
end;

procedure TJvInterpreterUnit.Run;
var
  FunDesc: TJvInterpreterFunDesc;
begin
  Init;
  NextToken;
  case TTyp of
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
  end;
  FCompiled := True;
end;

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
end;

procedure TJvInterpreterUnit.SourceChanged;
begin
  inherited SourceChanged;
end;

function TJvInterpreterUnit.GetValue(Identifier: string; var Value: Variant;
  var Args: TJvInterpreterArgs): Boolean;
var
  FunDesc: TJvInterpreterFunDesc;
  OldArgs: TJvInterpreterArgs;
begin
  Result := inherited GetValue(Identifier, Value, Args);
  if Result then
    Exit;
  if Args.Obj = nil then
    FunDesc := FAdapter.FindFunDesc(FCurUnitName, Identifier)
  else if (Args.Obj is TJvInterpreterSrcUnit) then
    FunDesc := FAdapter.FindFunDesc((Args.Obj as TJvInterpreterSrcUnit).Identifier,
      Identifier)
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
end;

function TJvInterpreterUnit.SetValue(Identifier: string; const Value: Variant;
  var Args: TJvInterpreterArgs): Boolean;
begin
  Result := inherited SetValue(Identifier, Value, Args);
end;

function TJvInterpreterUnit.GetUnitSource(UnitName: string; var Source: string): Boolean;
begin
  Result := False;
  if Assigned(FOnGetUnitSource) then
    FOnGetUnitSource(UnitName, Source, Result);
end;

procedure TJvInterpreterUnit.DeclareExternalFunction(const Declaration: string);
var
  OldSource: string;
  OldPos: Integer;
begin
  Source := Declaration;
  OldSource := Source;
  OldPos := Parser.Pos;
  try
    NextToken;
    if not (TTyp in [ttFunction, ttProcedure]) then
      ErrorExpected('''' + kwFUNCTION + ''' or ''' + kwPROCEDURE + '''');
    Function1;
  finally
    Source := OldSource;
    Parser.Pos := OldPos;
  end;
end;

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
  finally
    if not Cmp(FCurUnitName, OldUnitName) then
    begin
      FCurUnitName := OldUnitName;
      FAdapter.CurUnitChanged(FCurUnitName, S);
      Source := S;
    end;
    PopState;
  end;
end;

function TJvInterpreterUnit.CallFunction(const FunName: string; Args: TJvInterpreterArgs;
  Params: array of Variant): Variant;
begin
  Result := CallFunctionEx(nil, '', FunName, Args, Params);
end;

function TJvInterpreterUnit.CallFunctionEx(Instance: TObject; const UnitName: string;
  const FunName: string; Args: TJvInterpreterArgs; Params: array of Variant): Variant;
var
  FunDesc: TJvInterpreterFunDesc;
  i: Integer;
  OldArgs: TJvInterpreterArgs;
  OldInstance: TObject;
begin
  if not FCompiled then
    Compile;
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
          Inc(Self.Args.Count);
        end;
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
      JvInterpreterErrorN(ieUnknownIdentifier, -1, FunName);
  finally
    FCurInstance := OldInstance;
  end;
end;

function TJvInterpreterUnit.FunctionExists(const UnitName: string; const FunName: string)
  : Boolean;
begin
  Result := FAdapter.FindFunDesc(UnitName, FunName) <> nil;
end;

//=== TJvInterpreterProgramStrings ===========================================

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

//=== TJvInterpreterProgram ==================================================

constructor TJvInterpreterProgram.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPas := TJvInterpreterProgramStrings.Create;
  (FPas as TJvInterpreterProgramStrings).FJvInterpreterProgram := Self;
end;

destructor TJvInterpreterProgram.Destroy;
begin
  FPas.Free;
  inherited Destroy;
end;

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
    case TTyp of
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
    end;
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
end;

{$IFDEF JvInterpreter_OLEAUTO}
var
  OleInitialized: Boolean;
{$ENDIF JvInterpreter_OLEAUTO}

function TJvInterpreterFunction.GetDebugPointerToGlobalVars: TJvInterpreterVarList;
begin
  Result := Adapter.FSrcVarList;
end;

function TJvInterpreterFunction.GetDebugPointerToFunStack: Pointer;
begin
  Result := FunStack;
end;


{ TJvInterpreterMethodList }

procedure TJvInterpreterMethodList.Sort(Compare: TListSortCompare);

  function SortIdentifier(Item1, Item2: Pointer): Integer;
  begin
{ function AnsiStrIComp about 30% faster when AnsiCompareText }
{ Result := AnsiCompareText(TJvInterpreterIdentifier(Item1).Identifier,
TJvInterpreterIdentifier(Item2).Identifier); }
    Result := AnsiStrIComp(PChar(TJvInterpreterIdentifier(Item1).Identifier),
      PChar(TJvInterpreterIdentifier(Item2).Identifier));

    if (Result = 0) and (Item1 <> Item2) then
    begin
      if TJvInterpreterMethod(Item1).FClassType.InheritsFrom(TJvInterpreterMethod(Item2).FClassType) then
        Result := -1
      else if TJvInterpreterMethod(Item2).FClassType.InheritsFrom(TJvInterpreterMethod(Item1).FClassType) then
        Result := 1;
    end;
  end;

begin
  inherited Sort(@SortIdentifier);
end;

{ TJvInterpreterRecord }

procedure TJvInterpreterRecord.AddField(UnitName, Identifier, Typ: string;
  VTyp: Word; const Value: Variant; DataType: IJvInterpreterDataType);
begin
  Fields[FieldCount].Identifier := Identifier;
  Fields[FieldCount].Typ := varEmpty;
  Fields[FieldCount].Offset := RecordSize;
  Fields[FieldCount].DataType := DataType;

  Inc(RecordSize, SizeOf(TVarData));
  Inc(FieldCount);
end;

procedure TJvInterpreterRecord.NewRecord(var Value: Variant);
var
  i: integer;
  Rec: PChar;
//  Res: Boolean;
  RecHolder: TJvInterpreterRecHolder;
begin
  if Assigned(CreateFunc) then
    CreateFunc(Pointer(Rec))
  else
  begin
    GetMem(Rec, RecordSize);
    for i := 0 to FieldCount - 1 do
    begin
      if Fields[i].Typ = varString then
        PString(PString(Rec + Fields[i].Offset)^) := @EmptyStr
      else if Fields[i].Typ = varEmpty then
      begin
        PVarData(Rec + Fields[i].Offset)^.VType := varNull;
        if Fields[i].DataType <> nil then
          Fields[i].DataType.Init(Variant(PVarData(Rec + Fields[i].Offset)^));
      end;
    end;
  end;
  JvInterpreterVarCopy(Value, R2V(Identifier, Rec));
  RecHolder := TJvInterpreterRecHolder(TVarData(Value).vPointer);
  RecHolder.JvInterpreterRecord := Self;
end;

{ TJvInterpreterRecordDataType }

constructor TJvInterpreterRecordDataType.Create(
  ARecordDesc: TJvInterpreterRecord);
begin
  inherited Create;
  FRecordDesc := ARecordDesc;

end;

function TJvInterpreterRecordDataType.GetTyp: Word;
begin
  Result := varEmpty;
end;

procedure TJvInterpreterRecordDataType.Init(var V: Variant);
//var
//  i: integer;
begin
  FRecordDesc.NewRecord(V);
end;

{ TJvInterpreterArrayDataType }

constructor TJvInterpreterArrayDataType.Create(AArrayBegin,
  AArrayEnd: TJvInterpreterArrayValues; ADimension: Integer; AArrayType: Integer; ADT: IJvInterpreterDataType);
begin
  inherited Create;
  FArrayBegin := AArrayBegin;
  FArrayEnd := AArrayEnd;
  FDimension := ADimension;
  FArrayType := AArrayType;
  FDT := ADT;
end;

function TJvInterpreterArrayDataType.GetTyp: Word;
begin
  Result := varArray;
end;

procedure TJvInterpreterArrayDataType.Init(var V: Variant);
begin
  V := Integer(JvInterpreterArrayInit(FDimension, FArrayBegin, FArrayEnd,
    FArrayType, FDT));
  TVarData(V).VType := varArray;
end;

{ TJvInterpreterSimpleDataType }

constructor TJvInterpreterSimpleDataType.Create(ATyp: TVarType);
begin
  inherited Create;
  FTyp := ATyp;
end;

function TJvInterpreterSimpleDataType.GetTyp: Word;
begin
  Result := FTyp;
end;

procedure TJvInterpreterSimpleDataType.Init(var V: Variant);
begin
  V := Null;
  TVarData(V).VType := varEmpty;
  if FTyp <> 0 then
    V := Var2Type(V, FTyp);
end;

initialization
{$IFDEF COMPILER6_UP}
  VariantRecordInstance := TJvRecordVariantType.Create;
  VariantObjectInstance := TJvObjectVariantType.Create;
  VariantClassInstance := TJvClassVariantType.Create;
  VariantPointerInstance := TJvPointerVariantType.Create;
  VariantSetInstance := TJvSetVariantType.Create;
  VariantArrayInstance := TJvArrayVariantType.Create;
{$ENDIF COMPILER6_UP}

  GlobalJvInterpreterAdapter := TJvInterpreterAdapter.Create(nil);
{$IFDEF JvInterpreter_OLEAUTO}
  OleInitialized := OleInitialize(nil) = S_OK;
{$ENDIF JvInterpreter_OLEAUTO}

finalization
{$IFDEF JvInterpreter_OLEAUTO}
  if OleInitialized then
    OleUnInitialize;
{$ENDIF JvInterpreter_OLEAUTO}
{$IFDEF JvInterpreter_DEBUG}
  if ObjCount <> 0 then
    Windows.MessageBox(0, PChar('Memory leak in JvInterpreter.pas'#13 +
      'ObjCount = ' + IntToStr(ObjCount)),
      'JvInterpreter Internal Error', MB_ICONERROR);
{$ENDIF}
  GlobalJvInterpreterAdapter.Free;

{$IFDEF COMPILER6_UP}
  FreeAndNil(VariantRecordInstance);
  FreeAndNil(VariantObjectInstance);
  FreeAndNil(VariantClassInstance);
  FreeAndNil(VariantPointerInstance);
  FreeAndNil(VariantSetInstance);
  FreeAndNil(VariantArrayInstance);
{$ENDIF COMPILER6_UP}

end.
